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
      ; valid : 'a [@rtlprefix "input_"]
      ; registers : 'a Registers.For_writeback.t [@rtlprefix "input_"]
      ; read_bus : 'a Memory.Read_bus.Dest.t
      ; read_response : 'a Memory.Read_response.With_valid.t
      }
    [@@deriving hardcaml ~rtlmangle:"$"]
  end

  module O = struct
    type 'a t =
      { valid : 'a
      ; registers : 'a Registers.For_writeback.t
      ; instruction : 'a [@bits 32]
      ; read_bus : 'a Memory.Read_bus.Source.t
      }
    [@@deriving hardcaml ~rtlmangle:"$"]
  end

  let create scope (i : _ I.t) =
    let reg_spec_with_clear = Reg_spec.create ~clock:i.clock ~clear:i.clear () in
    let%hw fetching =
      i.valid
      |: reg_fb
           ~width:1
           ~f:(fun t -> mux2 i.read_bus.ready gnd (t |: i.valid))
           reg_spec_with_clear
    in
    let registers =
      Registers.For_writeback.Of_signal.reg
        ~enable:i.valid
        reg_spec_with_clear
        i.registers
    in
    let aligned_address =
      (Memory.byte_address_to_memory_address (mux2 i.valid i.registers.pc registers.pc))
        .value
    in
    { O.read_bus =
        { Memory.Read_bus.Source.valid = fetching; data = { address = aligned_address } }
    ; valid = i.read_response.valid
    ; registers
    ; instruction = i.read_response.value.read_data
    }
  ;;

  let hierarchical (scope : Scope.t) (input : Signal.t I.t) =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ~scope ~name:"fetch" create input
  ;;
end
