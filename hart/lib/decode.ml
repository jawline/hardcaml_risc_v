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
      { clock : 'a Clocking.t
      ; valid : 'a [@rtlprefix "input_"]
      ; registers : 'a Registers.For_writeback.t [@rtlprefix "input_"]
      ; instruction : 'a [@bits 32]
      }
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t =
      { valid : 'a
      ; instruction : 'a Decoded_instruction.t
      }
    [@@deriving hardcaml]
  end

  module Without_registers = struct
    let create scope (i : _ I.t) =
      { O.valid = i.valid
      ; instruction =
          Decoded_instruction.of_instruction
            i.instruction
            (Registers.For_writeback.to_registers i.registers)
            scope
      }
    ;;

    let hierarchical (scope : Scope.t) (input : Signal.t I.t) =
      let module H = Hierarchy.In_scope (I) (O) in
      H.hierarchical ~scope ~name:"decode" create input
    ;;
  end

  let create scope (i : _ I.t) =
    let reg_spec_with_clear = Clocking.to_spec i.clock in
    let reg_spec_no_clear = Clocking.to_spec i.clock in
    let o = Without_registers.hierarchical scope i in
    if Hart_config.register_decode_output
    then
      { O.valid = reg reg_spec_with_clear o.valid
      ; instruction =
          Decoded_instruction.Of_signal.reg
            ~enable:o.valid
            reg_spec_no_clear
            o.instruction
      }
    else o
  ;;

  let hierarchical (scope : Scope.t) (input : Signal.t I.t) =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ~scope ~name:"decode" create input
  ;;
end
