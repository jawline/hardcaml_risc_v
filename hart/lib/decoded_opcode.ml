open Core
open Hardcaml
open Signal

type t =
  | ALU
  | Assign_pc_sum_of_arguments
  | Branch
  | Load
  | Store
  | Fence
  | System
[@@deriving compare, sexp, enumerate, variants]

(** Onehot encoded representation of the compressed opcode. *)
module Packed = struct
  type nonrec base = t

  let max_rank = Variants.to_rank System
  let width = num_bits_to_represent (1 lsl max_rank)

  type 'a t = { packed : 'a [@bits width] } [@@deriving hardcaml]
end

let valid (t : _ Packed.t) opcode = t.packed.:(Variants.to_rank opcode)
let construct_onehot ~f = { Packed.packed = all |> List.map ~f |> concat_lsb }

let construct_onehot_bits ~(f : t -> Bits.t) =
  { Packed.packed = all |> List.map ~f |> Bits.concat_lsb }
;;
