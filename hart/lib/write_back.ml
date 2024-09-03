open! Core
open Hardcaml
open Signal
open Hardcaml_memory_controller

module Make
    (Hart_config : Hart_config_intf.S)
    (Memory : Memory_bus_intf.S)
    (Registers : Registers_intf.S)
    (Decoded_instruction : Decoded_instruction_intf.M(Registers).S)
    (Transaction : Transaction_intf.S) =
struct
  module Op = Op.Make (Hart_config)
  module Branch = Branch.Make (Hart_config)
  module Load = Load.Make (Hart_config) (Memory)
  module Store = Store.Make (Hart_config) (Memory)

  let register_width = Register_width.bits Hart_config.register_width

  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; valid : 'a
      ; registers : 'a Registers.For_writeback.t
      ; instruction : 'a Decoded_instruction.t
      ; transaction : 'a Transaction.t
      ; error : 'a
      }
    [@@deriving hardcaml ~rtlmangle:"$"]
  end

  module O = struct
    type 'a t =
      { valid : 'a
      ; registers : 'a Registers.For_writeback.t
      ; error : 'a
      }
    [@@deriving hardcaml ~rtlmangle:"$"]
  end

  let create _scope (i : _ I.t) =
    let reg_spec_with_clear = Reg_spec.create ~clock:i.clock ~clear:i.clear () in
    let commit_transaction ~new_pc ~set_rd ~new_rd reg =
      let reg = Registers.For_writeback.to_registers reg in
      Registers.set_pc reg new_pc
      |> Registers.assign_when
           ~when_:set_rd
           ~index_signal:i.instruction.rd
           ~value_signal:new_rd
      |> Registers.For_writeback.of_registers
    in
    { O.valid = reg reg_spec_with_clear i.valid
    ; registers =
        Registers.For_writeback.Of_signal.reg
          reg_spec_with_clear
          (commit_transaction
             ~new_pc:i.transaction.new_pc
             ~set_rd:i.transaction.set_rd
             ~new_rd:i.transaction.new_rd
             i.registers)
    ; error = reg reg_spec_with_clear ~enable:i.valid i.error
    }
  ;;

  let hierarchical (scope : Scope.t) (input : Signal.t I.t) =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ~scope ~name:"write_back" create input
  ;;
end
