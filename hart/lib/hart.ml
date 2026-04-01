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
      { clock : 'a Clocking.t
      ; ecall_transaction : 'a Transaction.With_valid.t
      ; read_instruction : 'a Memory.Read_bus.Dest.t
      ; read_instruction_response : 'a Memory.Read_response.With_valid.t
      ; read_data : 'a Memory.Read_bus.Dest.t
      ; read_data_response : 'a Memory.Read_response.With_valid.t
      ; write_data : 'a Memory.Write_bus.Dest.t
      ; write_data_response : 'a Memory.Write_response.With_valid.t
      }
    [@@deriving hardcaml ~rtlmangle:"$", fields ~getters]
  end

  module O = struct
    type 'a t =
      { registers : 'a Registers.t
      ; is_ecall : 'a
      ; read_instruction : 'a Memory.Read_bus.Source.t
      ; read_data : 'a Memory.Read_bus.Source.t
      ; write_data : 'a Memory.Write_bus.Source.t
      }
    [@@deriving hardcaml ~rtlmangle:"$", fields ~getters]
  end

  let create scope (i : _ I.t) =
    let reg_spec_with_clear = Clocking.to_spec i.clock in
    let reg_spec_no_clear = Clocking.to_spec_no_clear i.clock in
    let%hw executor_finished = wire 1 in
    let executor_registers = Registers.For_writeback.Of_signal.wires () in
    let%hw just_cleared =
      ~:(i.clock.clear)
      &: reg ~initialize_to:Bits.vdd ~clear_to:vdd reg_spec_with_clear gnd
    in
    let executor =
      Execute_pipeline.hierarchical
        scope
        { Execute_pipeline.I.clock = i.clock
        ; valid = executor_finished |: reg reg_spec_no_clear just_cleared
        ; registers = executor_registers
        ; instret = executor_finished
        ; ecall_transaction = i.ecall_transaction
        ; read_instruction = i.read_instruction
        ; read_instruction_response = i.read_instruction_response
        ; read_data = i.read_data
        ; read_data_response = i.read_data_response
        ; write_data = i.write_data
        ; write_data_response = i.write_data_response
        }
    in
    executor_finished <-- executor.valid;
    Registers.For_writeback.Of_signal.(executor_registers <-- executor.registers);
    { O.registers = executor.registers |> Registers.For_writeback.to_registers
    ; is_ecall = executor.is_ecall
    ; read_instruction = executor.read_instruction
    ; read_data = executor.read_data
    ; write_data = executor.write_data
    }
  ;;

  let hierarchical ?instance (scope : Scope.t) (input : Signal.t I.t) =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ?instance ~scope ~name:"hart" create input
  ;;
end
