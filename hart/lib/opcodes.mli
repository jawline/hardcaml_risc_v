open! Core
open Hardcaml

type t =
  | Op
  | Op_imm
  | Jal
  | Jalr
  | Lui
  | Auipc
  | Branch
  | Load
  | Store
  | Fence
  | System
[@@deriving compare, sexp, enumerate, variants]

val to_int_repr : t -> int
val matches : Signal.t -> t -> Signal.t
