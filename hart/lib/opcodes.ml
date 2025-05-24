open Core
open Hardcaml
open Signal

module T = struct
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
end

include T
include Comparable.Make (T)

let construct_opcode col row =
  ((col land 0b111) lsl 2) lor ((row land 0b11) lsl 5) lor 0b11
;;

let to_int_repr = function
  | Op -> construct_opcode 0b100 0b01
  | Op_imm -> construct_opcode 0b100 0b00
  | Jal -> construct_opcode 0b011 0b11
  | Jalr -> construct_opcode 0b001 0b11
  | Lui -> construct_opcode 0b101 0b01
  | Auipc -> construct_opcode 0b101 0b00
  | Branch -> construct_opcode 0b000 0b11
  | Load -> construct_opcode 0b000 0b00
  | Store -> construct_opcode 0b000 0b01
  | Fence -> construct_opcode 0b011 0b00
  | System -> construct_opcode 0b100 0b11
;;

let matches signal k = signal ==:. to_int_repr k
