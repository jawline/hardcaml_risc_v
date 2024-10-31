(** An implementation of a one in flight pipeline for execution of Risc-V
    instructions. *)

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
      ; registers : 'a Registers.For_writeback.t
      ; ecall_transaction : 'a Transaction.t
      ; write_bus : 'a Memory.Write_bus.Rx.t [@rtlprefix "write$"]
      ; read_bus : 'a Memory.Read_bus.Rx.t [@rtlprefix "read$"]
      ; write_response : 'a Memory.Write_response.With_valid.t
           [@rtlprefix "write_response$"]
      ; read_response : 'a Memory.Read_response.With_valid.t [@rtlprefix "read_response$"]
      }
    [@@deriving hardcaml ~rtlmangle:"$"]
  end

  module O = struct
    type 'a t =
      { valid : 'a
      ; registers : 'a Registers.For_writeback.t
      ; is_ecall : 'a
      ; error : 'a
      ; write_bus : 'a Memory.Write_bus.Tx.t [@rtlprefix "write$"]
      ; read_bus : 'a Memory.Read_bus.Tx.t [@rtlprefix "read$"]
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
        ; read_bus = i.read_bus
        ; read_response = i.read_response
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
        ; read_bus = i.read_bus
        ; read_response = i.read_response
        ; write_bus = i.write_bus
        ; write_response = i.write_response
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
    ; read_bus =
        (let combine = Memory.Read_bus.Tx.map2 ~f:( |: ) in
         let gate (t : _ Memory.Read_bus.Tx.t) =
           Memory.Read_bus.Tx.Of_signal.mux2
             t.valid
             t
             (Memory.Read_bus.Tx.Of_signal.of_int 0)
         in
         combine (gate fetch.read_bus) (gate execute.read_bus))
    ; write_bus = execute.write_bus
    ; is_ecall = execute.is_ecall
    }
  ;;

  let hierarchical (scope : Scope.t) (input : Signal.t I.t) =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ~scope ~name:"execute_pipeline" create input
  ;;
end
