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
      ; valid : 'a
      ; registers : 'a Registers.For_writeback.t [@rtlprefix "input_registers$"]
      ; ecall_transaction : 'a Transaction.t
      ; write_bus : 'a Memory.Write_bus.Rx.t list
           [@rtlprefix "write$"] [@length required_write_channels]
      ; write_response : 'a Memory.Write_response.With_valid.t list
           [@rtlprefix "write_response$"] [@length required_write_channels]
      ; read_bus : 'a Memory.Read_bus.Rx.t list
           [@rtlprefix "read$"] [@length required_read_channels]
      ; read_response : 'a Memory.Read_response.With_valid.t list
           [@rtlprefix "read_response$"] [@length required_read_channels]
      }
    [@@deriving hardcaml ~rtlmangle:"$"]
  end

  module O = struct
    type 'a t =
      { valid : 'a [@rtlname "output_valid"]
      ; registers : 'a Registers.For_writeback.t [@rtlprefix "output_registers$"]
      ; is_ecall : 'a
      ; error : 'a
      ; write_bus : 'a Memory.Write_bus.Tx.t list
           [@rtlprefix "write$"] [@length required_write_channels]
      ; read_bus : 'a Memory.Read_bus.Tx.t list
           [@rtlprefix "read$"] [@length required_read_channels]
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
        ; error = fetch.error
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
        ; ecall_transaction = i.ecall_transaction
        ; error = decode.error
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
        ; error = execute.error
        }
    in
    { O.valid = write_back.valid
    ; registers = write_back.registers
    ; error = write_back.error
    ; read_bus = [ execute.read_bus; fetch.read_bus ]
    ; write_bus = [ execute.write_bus ]
    ; is_ecall = execute.is_ecall
    }
  ;;

  let hierarchical (scope : Scope.t) (input : Signal.t I.t) =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ~scope ~name:"execute_pipeline" create input
  ;;
end
