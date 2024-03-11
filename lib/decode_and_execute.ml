open! Core
open Hardcaml
open Signal
open Always

module Make
    (Hart_config : Hart_config_intf.S)
    (Memory : Memory_bus_intf.S)
    (Registers : Registers_intf.S) =
struct
  module I = struct
    type 'a t =
      { memory_controller_to_hart : 'a Memory.Rx_bus.Tx.t
           [@rtlprefix "memory_controller_to_hart"]
      ; hart_to_memory_controller : 'a Memory.Tx_bus.Rx.t
           [@rtlprefix "hart_to_memory_controller"]
      ; enable : 'a
      ; instruction : 'a
           (* TODO: This is assuming Rv32i, I guess in practice this should be the length of the longest instruction we support? *)
           [@bits 32]
      ; registers : 'a Registers.t
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t =
      { memory_controller_to_hart : 'a Memory.Rx_bus.Rx.t
           [@rtlprefix "memory_controller_to_hart"]
      ; hart_to_memory_controller : 'a Memory.Tx_bus.Tx.t
           [@rtlprefix "hart_to_memory_controller"]
      ; finished : 'a
      ; new_registers : 'a Registers.t
      }
    [@@deriving sexp_of, hardcaml]
  end

  let op_instructions registers instruction = [] |> proc

  let create _scope (_i : _ I.t) =
    let finished = Variable.wire ~default:(zero 1) in
    let new_registers = Registers.Of_always.wire zero in
    compile
      [ when_
          enable
          [ when_
              (Decoder.opcode i.instruction =:. Opcodes.op)
              [ Registers.Of_always.assign new_registers (op i.registers i.instructions)
              ; finished <--. 1
              ]
          ]
      ];
    { O.memory_controller_to_hart = Memory.Rx_bus.Rx.Of_signal.of_int 1
    ; hart_to_memory_controller = Memory.Tx_bus.Tx.of_int 1
    ; finished = finished.value
    ; new_registers = Registers.Of_always.value new_registers
    }
  ;;

  let hierarchical ~instance (scope : Scope.t) (input : Signal.t I.t) =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ~scope ~name:"Decode_and_execute" ~instance create input
  ;;
end
