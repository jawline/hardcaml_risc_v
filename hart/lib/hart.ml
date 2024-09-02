(** An implementation of a one in flight Risc-V Hart. On clear the execution
    pipeline is started.  It is restarted with the new state of the registers
    each time the execution pipeline pulses a valid signal with a new set of
    registers. *)
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
      ; write_bus : 'a Memory.Write_bus.Rx.t [@rtlprefix "write$"]
      ; read_bus : 'a Memory.Read_bus.Rx.t [@rtlprefix "read$"]
      ; write_response : 'a Memory.Write_response.With_valid.t
           [@rtlprefix "write_response$"]
      ; read_response : 'a Memory.Read_response.With_valid.t [@rtlprefix "read_response$"]
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
      ; write_bus : 'a Memory.Write_bus.Tx.t [@rtlprefix "write$"]
      ; read_bus : 'a Memory.Read_bus.Tx.t [@rtlprefix "read$"]
      }
    [@@deriving sexp_of, hardcaml ~rtlmangle:"$"]
  end

  let create scope (i : _ I.t) =
    let reg_spec_with_clear = Reg_spec.create ~clock:i.clock ~clear:i.clear () in
    let%hw executor_finished = wire 1 in
    let executor_registers = Registers.For_writeback.Of_signal.wires () in
    let%hw restarting =
      reg_fb
        ~width:1
        ~f:(fun _t -> gnd)
        (Reg_spec.override ~clear_to:vdd reg_spec_with_clear)
    in
    let executor =
      Execute_pipeline.hierarchical
        scope
        { Execute_pipeline.I.clock = i.clock
        ; clear = i.clear
        ; valid = executor_finished |: restarting
        ; registers =
            Registers.For_writeback.Of_signal.mux2
              restarting
              (Registers.For_writeback.Of_signal.of_int 0)
              executor_registers
        ; ecall_transaction = i.ecall_transaction
        ; read_bus = i.read_bus
        ; read_response = i.read_response
        ; write_bus = i.write_bus
        ; write_response = i.write_response
        }
    in
    executor_finished <== (executor.valid &: ~:(executor.error));
    Registers.For_writeback.Of_signal.(executor_registers <== executor.registers);
    { O.registers =
        Registers.For_writeback.Of_signal.reg
          ~enable:(executor.valid &: ~:(executor.error))
          reg_spec_with_clear
          executor.registers
        |> Registers.For_writeback.to_registers
    ; error = reg ~enable:executor.valid reg_spec_with_clear executor.error
    ; is_ecall = executor.is_ecall
    ; read_bus = executor.read_bus
    ; write_bus = executor.write_bus
    }
  ;;

  let hierarchical ~instance (scope : Scope.t) (input : Signal.t I.t) =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ~scope ~name:"hart" ~instance create input
  ;;
end
