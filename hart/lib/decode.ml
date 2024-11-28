open! Core
open Hardcaml
open Signal

module Make
    (Hart_config : Hart_config_intf.S)
    (Registers : Registers_intf.S)
    (Decoded_instruction : Decoded_instruction_intf.M(Registers).S) =
struct
  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; valid : 'a
      ; registers : 'a Registers.For_writeback.t [@rtlprefix "input_registers$"]
      ; instruction : 'a [@bits 32]
      ; error : 'a
      }
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t =
      { valid : 'a [@rtlname "output_valid"]
      ; registers : 'a Registers.For_writeback.t [@rtlprefix "output_registers$"]
      ; instruction : 'a Decoded_instruction.t
      ; error : 'a [@rtlname "output_error"]
      }
    [@@deriving hardcaml]
  end

  let create scope (i : _ I.t) =
    let reg_spec_with_clear = Reg_spec.create ~clock:i.clock ~clear:i.clear () in
    let reg_spec_no_clear = Reg_spec.create ~clock:i.clock () in
    { O.valid = reg reg_spec_with_clear i.valid
    ; registers =
        Registers.For_writeback.Of_signal.reg
          ~enable:i.valid
          reg_spec_no_clear
          i.registers
    ; instruction =
        Decoded_instruction.Of_signal.reg
          ~enable:i.valid
          reg_spec_no_clear
          (Decoded_instruction.of_instruction
             i.instruction
             (Registers.For_writeback.to_registers i.registers)
             scope)
    ; error = reg reg_spec_with_clear i.error
    }
  ;;

  let hierarchical (scope : Scope.t) (input : Signal.t I.t) =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ~scope ~name:"decode" create input
  ;;
end
