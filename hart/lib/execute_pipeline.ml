(** An implementation of a one in flight pipeline for execution of Risc-V
    instructions. *)

open! Core
open Hardcaml
open Hardcaml_memory_controller

module Make
    (Hart_config : Hart_config_intf.S)
    (Memory : Memory_bus_intf.S)
    (Registers : Registers_intf.S)
    (Decoded_instruction : Decoded_instruction_intf.M(Registers).S)
    (Transaction : Transaction_intf.S) =
struct
  module Fetch = Fetch.Make (Hart_config) (Memory) (Registers)
  module Decode = Decode.Make (Hart_config) (Registers) (Decoded_instruction)

  module Execute =
    Execute.Make (Hart_config) (Memory) (Registers) (Decoded_instruction) (Transaction)

  module Write_back =
    Write_back.Make (Hart_config) (Memory) (Registers) (Decoded_instruction) (Transaction)

  module I = struct
    type 'a t =
      { clock : 'a Clocking.t
      ; valid : 'a [@rtlprefix "input_"]
      ; registers : 'a Registers.For_writeback.t [@rtlprefix "input_"]
      ; ecall_transaction : 'a Transaction.With_valid.t
      ; instret : 'a
      ; read_instruction : 'a Memory.Read_bus.Dest.t
      ; read_instruction_response : 'a Memory.Read_response.With_valid.t
      ; read_data : 'a Memory.Read_bus.Dest.t
      ; read_data_response : 'a Memory.Read_response.With_valid.t
      ; write_data : 'a Memory.Write_bus.Dest.t
      ; write_data_response : 'a Memory.Write_response.With_valid.t
      }
    [@@deriving hardcaml ~rtlmangle:"$"]
  end

  module O = struct
    type 'a t =
      { valid : 'a
      ; registers : 'a Registers.For_writeback.t
      ; is_ecall : 'a
      ; fault : 'a
      ; read_instruction : 'a Memory.Read_bus.Source.t
      ; read_data : 'a Memory.Read_bus.Source.t
      ; write_data : 'a Memory.Write_bus.Source.t
      }
    [@@deriving hardcaml ~rtlmangle:"$"]
  end

  let create scope (i : _ I.t) =
    let fetch =
      Fetch.hierarchical
        scope
        { Fetch.I.clock = i.clock
        ; valid = i.valid
        ; pc = i.registers.pc
        ; read_bus = i.read_instruction
        ; read_response = i.read_instruction_response
        }
    in
    let decode =
      Decode.hierarchical
        scope
        { Decode.I.clock = i.clock
        ; valid = fetch.valid
        ; registers = i.registers
        ; instruction = fetch.instruction
        }
    in
    let execute =
      Execute.hierarchical
        scope
        { Execute.I.clock = i.clock
        ; valid = decode.valid
        ; instruction = decode.instruction
        ; instret = i.instret
        ; ecall_transaction = i.ecall_transaction
        ; read_bus = i.read_data
        ; read_response = i.read_data_response
        ; write_bus = i.write_data
        ; write_response = i.write_data_response
        }
    in
    let write_back =
      Write_back.hierarchical
        scope
        { Write_back.I.clock = i.clock
        ; valid = execute.valid
        ; registers = i.registers
        ; instruction = execute.instruction
        ; transaction = execute.transaction
        }
    in
    { O.valid = write_back.valid
    ; registers = write_back.registers
    ; read_instruction = fetch.read_bus
    ; read_data = execute.read_bus
    ; write_data = execute.write_bus
    ; is_ecall = execute.is_ecall
    ; fault = execute.error
    }
  ;;

  let hierarchical (scope : Scope.t) (input : Signal.t I.t) =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ~scope ~name:"execute_pipeline" create input
  ;;
end
