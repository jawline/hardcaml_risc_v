open! Core
open Hardcaml

type t =
  | ALU
  | Assign_pc_sum_of_arguments
  | Lui
  | Branch
  | Load
  | Store
  | Fence
  | System
[@@deriving compare, sexp, enumerate, variants]

(** Onehot encoded representation of the compressed opcode. *)
module Packed : sig
  type nonrec base = t
  type 'a t = { packed : 'a [@bits width] } [@@deriving hardcaml]

  val width : int
end

val valid : Signal.t Packed.t -> t -> Signal.t
val construct_onehot : f:(t -> Signal.t) -> Signal.t Packed.t
val construct_onehot_bits : f:(t -> Bits.t) -> Bits.t Packed.t
