(** An implementation of a one in flight pipeline for execution of Risc-V
    instructions. *)

open! Core
open Hardcaml
open Hardcaml_memory_controller

let required_read_channels = 2
let required_write_channels = 1

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
      { clock : 'a
      ; clear : 'a
      ; valid : 'a [@rtlprefix "input_"]
      ; registers : 'a Registers.For_writeback.t [@rtlprefix "input_"]
      ; ecall_transaction : 'a Transaction.With_valid.t
      ; instret : 'a
      ; write_bus : 'a Memory.Write_bus.Dest.t list [@length required_write_channels]
      ; write_response : 'a Memory.Write_response.With_valid.t list
            [@length required_write_channels]
      ; read_bus : 'a Memory.Read_bus.Dest.t list [@length required_read_channels]
      ; read_response : 'a Memory.Read_response.With_valid.t list
            [@length required_read_channels]
      }
    [@@deriving hardcaml ~rtlmangle:"$"]
  end

  module O = struct
    type 'a t =
      { valid : 'a
      ; registers : 'a Registers.For_writeback.t
      ; is_ecall : 'a
      ; write_bus : 'a Memory.Write_bus.Source.t list [@length required_write_channels]
      ; read_bus : 'a Memory.Read_bus.Source.t list [@length required_read_channels]
      ; fault : 'a
      }
    [@@deriving hardcaml ~rtlmangle:"$"]
  end

  let create scope (i : _ I.t) =
    let fetch =
      Fetch.hierarchical
        scope
        { Fetch.I.clock = i.clock
        ; clear = i.clear
        ; valid = i.valid
        ; registers = i.registers
        ; read_bus = List.nth_exn i.read_bus 1
        ; read_response = List.nth_exn i.read_response 1
        }
    in
    let decode =
      Decode.hierarchical
        scope
        { Decode.I.clock = i.clock
        ; clear = i.clear
        ; valid = fetch.valid
        ; registers = fetch.registers
        ; instruction = fetch.instruction
        }
    in
    let execute =
      Execute.hierarchical
        scope
        { Execute.I.clock = i.clock
        ; clear = i.clear
        ; valid = decode.valid
        ; registers = decode.registers
        ; instruction = decode.instruction
        ; instret = i.instret
        ; ecall_transaction = i.ecall_transaction
        ; read_bus = List.nth_exn i.read_bus 0
        ; read_response = List.nth_exn i.read_response 0
        ; write_bus = List.nth_exn i.write_bus 0
        ; write_response = List.nth_exn i.write_response 0
        }
    in
    let write_back =
      Write_back.hierarchical
        scope
        { Write_back.I.clock = i.clock
        ; clear = i.clear
        ; valid = execute.valid
        ; registers = execute.registers
        ; instruction = execute.instruction
        ; transaction = execute.transaction
        }
    in
    { O.valid = write_back.valid
    ; registers = write_back.registers
    ; read_bus = [ execute.read_bus; fetch.read_bus ]
    ; write_bus = [ execute.write_bus ]
    ; is_ecall = execute.is_ecall
    ; fault = execute.error
    }
  ;;

  let hierarchical (scope : Scope.t) (input : Signal.t I.t) =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ~scope ~name:"execute_pipeline" create input
  ;;
end
