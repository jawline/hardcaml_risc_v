open! Core
open Hardcaml
open Hardcaml_memory_controller
open Signal

module Make
    (Hart_config : Hart_config_intf.S)
    (Memory : Memory_bus_intf.S)
    (Registers : Registers_intf.S) =
struct
  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; memory_controller_to_hart : 'a Memory.Rx_bus.Tx.t
           [@rtlprefix "memory_controller_to_hart"]
      ; hart_to_memory_controller : 'a Memory.Tx_bus.Rx.t
           [@rtlprefix "hart_to_memory_controller"]
      ; valid : 'a
      ; registers : 'a Registers.For_writeback.t [@rtlprefix "registers$"]
      }
    [@@deriving sexp_of, hardcaml ~rtlmangle:"$"]
  end

  module O = struct
    type 'a t =
      { hart_to_memory_controller : 'a Memory.Tx_bus.Tx.t
           [@rtlprefix "hart_to_memory_controller"]
      ; valid : 'a
      ; registers : 'a Registers.For_writeback.t [@rtlprefix "registers$"]
      ; instruction : 'a [@bits 32]
      ; error : 'a
      }
    [@@deriving sexp_of, hardcaml ~rtlmangle:"$"]
  end

  let create scope (i : _ I.t) =
    let reg_spec_with_clear = Reg_spec.create ~clock:i.clock ~clear:i.clear () in
    let reg_spec_no_clear = Reg_spec.create ~clock:i.clock () in
    let%hw fetching =
      i.valid
      |: reg_fb
           ~width:1
           ~f:(fun t -> mux2 i.hart_to_memory_controller.ready gnd (t |: vdd))
           reg_spec_with_clear
    in
    let registers =
      Registers.For_writeback.Of_signal.reg ~enable:i.valid reg_spec_no_clear i.registers
    in
    { O.hart_to_memory_controller =
        { Memory.Tx_bus.Tx.valid = fetching
        ; data =
            { Memory.Tx_data.address = mux2 i.valid i.registers.pc registers.pc
            ; write = gnd
            ; write_data = zero 32
            }
        }
    ; valid = ~:fetching &: i.memory_controller_to_hart.valid
    ; registers
    ; instruction = i.memory_controller_to_hart.data.read_data
    ; error = i.memory_controller_to_hart.data.error
    }
  ;;

  let hierarchical (scope : Scope.t) (input : Signal.t I.t) =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ~scope ~name:"fetch" create input
  ;;
end
