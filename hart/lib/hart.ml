open! Core
open Hardcaml
open Hardcaml_memory_controller
open Signal

module Make
    (Hart_config : Hart_config_intf.S)
    (Memory : Memory_bus_intf.S)
    (Registers : Registers_intf.S)
    (Decoded_instruction : Decoded_instruction_intf.M(Registers).S)
    (Transaction : Transaction_intf.S) =
struct
  module Execute_pipeline =
    Execute_pipeline.Make (Hart_config) (Memory) (Registers) (Decoded_instruction)
      (Transaction)

  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; ecall_transaction : 'a Transaction.t
          (* When is_ecall is high the opcode will be considered finished when
             ecall_transaction is finished. If a user wants custom behaviour on ecall
             they should hold ecall finished low, do the work, then raise finished. *)
      ; memory_controller_to_hart : 'a Memory.Rx_bus.Tx.t
           [@rtlprefix "memory_controller_to_hart"]
      ; hart_to_memory_controller : 'a Memory.Tx_bus.Rx.t
           [@rtlprefix "hart_to_memory_controller"]
      }
    [@@deriving sexp_of, hardcaml ~rtlmangle:"$"]
  end

  module O = struct
    type 'a t =
      { registers : 'a Registers.t
      ; error : 'a
      ; is_ecall : 'a
      (** Set high when the hart is in an ecall and is delagating behaviour to
          the user design. *)
      ; hart_to_memory_controller : 'a Memory.Tx_bus.Tx.t
           [@rtlprefix "hart_to_memory_controller"]
      }
    [@@deriving sexp_of, hardcaml ~rtlmangle:"$"]
  end

  let create scope (i : _ I.t) =
    let reg_spec = Reg_spec.create ~clock:i.clock ~clear:i.clear () in
    let start_instruction = wire 1 in
    let registers = Registers.For_writeback.Of_signal.wires () in
    let executor =
      Execute_pipeline.hierarchical
        scope
        { Execute_pipeline.I.clock = i.clock
        ; clear = i.clear
        ; valid = start_instruction
        ; registers
        ; memory_controller_to_hart = i.memory_controller_to_hart
        ; hart_to_memory_controller = i.hart_to_memory_controller
        }
    in
    start_instruction
    <== reg_fb ~width:1 ~f:(fun _t -> gnd) (Reg_spec.override ~clear_to:vdd reg_spec);
    Registers.For_writeback.Of_signal.(
      registers <== reg reg_spec (mux2 executor.valid executor.registers registers));
    { O.registers = Registers.For_writeback.to_registers registers
    ; error = executor.error
    ; is_ecall = assert false
    ; hart_to_memory_controller = executor.hart_to_memory_controller
    }
  ;;

  let hierarchical ~instance (scope : Scope.t) (input : Signal.t I.t) =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ~scope ~name:"hart" ~instance create input
  ;;
end
