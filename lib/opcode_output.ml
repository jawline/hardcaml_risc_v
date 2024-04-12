(** This encodes all of the outputs of an opcode for Decode_and_execute
    including the memory interface. *)
open! Core

open! Hardcaml
open Hardcaml_memory_controller

module Make (Hart_config : Hart_config_intf.S) (Memory : Memory_bus_intf.S) = struct
  module Transaction = Transaction.Make (Hart_config) (Memory)

  type 'a t =
    { transaction : 'a Transaction.t
    ; memory_controller_to_hart : 'a Memory.Rx_bus.Rx.t
    ; hart_to_memory_controller : 'a Memory.Tx_bus.Tx.t
    }
  [@@deriving sexp_of, hardcaml]
end
