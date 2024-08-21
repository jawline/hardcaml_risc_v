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
      ; memory_controller_to_hart : 'a Memory.Rx_bus.Tx.t
      ; hart_to_memory_controller : 'a Memory.Tx_bus.Rx.t
      }
    [@@deriving hardcaml ~rtlmangle:"$"]
  end

  module O = struct
    type 'a t =
      { valid : 'a
      ; registers : 'a Registers.For_writeback.t
      ; error : 'a
      ; hart_to_memory_controller : 'a Memory.Tx_bus.Tx.t
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
        ; memory_controller_to_hart = i.memory_controller_to_hart
        ; hart_to_memory_controller = i.hart_to_memory_controller
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
        ; memory_controller_to_hart = i.memory_controller_to_hart
        ; hart_to_memory_controller = i.hart_to_memory_controller
        }
    in
    { O.valid = write_back.valid
    ; registers = write_back.registers
    ; error = fetch.error |: execute.error |: write_back.error
    ; hart_to_memory_controller =
        Memory.Tx_bus.Tx.map2
          ~f:( |: )
          fetch.hart_to_memory_controller
          write_back.hart_to_memory_controller
    }
  ;;

  let hierarchical (scope : Scope.t) (input : Signal.t I.t) =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ~scope ~name:"execute_pipeline" create input
  ;;
end