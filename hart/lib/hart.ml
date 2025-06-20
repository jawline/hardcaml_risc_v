open! Core
open Hardcaml
open Hardcaml_memory_controller
open Signal

let required_read_channels = Execute_pipeline.required_read_channels
let required_write_channels = Execute_pipeline.required_write_channels

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
      ; ecall_transaction : 'a Transaction.With_valid.t
      ; write_bus : 'a Memory.Write_bus.Dest.t list [@length required_write_channels]
      ; write_response : 'a Memory.Write_response.With_valid.t list
            [@length required_write_channels]
      ; read_bus : 'a Memory.Read_bus.Dest.t list [@length required_read_channels]
      ; read_response : 'a Memory.Read_response.With_valid.t list
            [@length required_read_channels]
      }
    [@@deriving hardcaml ~rtlmangle:"$", fields ~getters]
  end

  module O = struct
    type 'a t =
      { registers : 'a Registers.t
      ; is_ecall : 'a
      ; write_bus : 'a Memory.Write_bus.Source.t list [@length required_write_channels]
      ; read_bus : 'a Memory.Read_bus.Source.t list [@length required_read_channels]
      }
    [@@deriving hardcaml ~rtlmangle:"$", fields ~getters]
  end

  let create scope (i : _ I.t) =
    let reg_spec_with_clear = Reg_spec.create ~clock:i.clock ~clear:i.clear () in
    let reg_spec_no_clear = Reg_spec.create ~clock:i.clock () in
    let%hw executor_finished = wire 1 in
    let executor_registers = Registers.For_writeback.Of_signal.wires () in
    let%hw just_cleared = ~:(i.clear) &: reg reg_spec_no_clear i.clear in
    let executor =
      Execute_pipeline.hierarchical
        scope
        { Execute_pipeline.I.clock = i.clock
        ; clear = i.clear
        ; valid = executor_finished |: just_cleared
        ; registers = executor_registers
        ; instret = executor_finished
        ; ecall_transaction = i.ecall_transaction
        ; read_bus = i.read_bus
        ; read_response = i.read_response
        ; write_bus = i.write_bus
        ; write_response = i.write_response
        }
    in
    executor_finished <-- executor.valid;
    Registers.For_writeback.Of_signal.(executor_registers <-- executor.registers);
    { O.registers =
        Registers.For_writeback.Of_signal.reg
          ~enable:executor.valid
          reg_spec_with_clear
          executor.registers
        |> Registers.For_writeback.to_registers
    ; is_ecall = executor.is_ecall
    ; read_bus = executor.read_bus
    ; write_bus = executor.write_bus
    }
  ;;

  let hierarchical ?instance (scope : Scope.t) (input : Signal.t I.t) =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ?instance ~scope ~name:"hart" create input
  ;;
end
