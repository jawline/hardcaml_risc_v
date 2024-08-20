(** This encodes all of the outputs of an opcode for Decode_and_execute
    including the memory interface. *)
open! Core

open! Hardcaml
open Hardcaml_memory_controller

module Make
    (Hart_config : Hart_config_intf.S)
    (Memory : Memory_bus_intf.S)
    (Transaction : Transaction_intf.S) =
struct
  type 'a t =
    { valid : 'a
    ; transaction : 'a Transaction.t
    }
  [@@deriving sexp_of, hardcaml ~rtlmangle:"$"]
end
