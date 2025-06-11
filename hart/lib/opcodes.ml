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
    | System
  [@@deriving compare, sexp, enumerate, variants]
end

include T
include Comparable.Make (T)

let to_int_repr = function
  | Op -> 0b01_100_11
  | Op_imm -> 0b00_100_11
  | Jalr -> 0b11_001_11
  | Jal -> 0b11_011_11
  | Lui -> 0b01_101_11
  | Auipc -> 0b00_101_11
  | Branch -> 0b11_000_11
  | Load -> 0b00_000_11
  | Store -> 0b01_000_11
  | System -> 0b11_100_11
;;

let%expect_test "opcodes" =
  all
  |> List.iter ~f:(fun t -> print_s [%message "" ~_:(t : t) (to_int_repr t : Int.Hex.t)]);
  [%expect
    {|
    (Op ("to_int_repr t" 0x33))
    (Op_imm ("to_int_repr t" 0x13))
    (Jal ("to_int_repr t" 0x6f))
    (Jalr ("to_int_repr t" 0x67))
    (Lui ("to_int_repr t" 0x37))
    (Auipc ("to_int_repr t" 0x17))
    (Branch ("to_int_repr t" 0x63))
    (Load ("to_int_repr t" 0x3))
    (Store ("to_int_repr t" 0x23))
    (System ("to_int_repr t" 0x73))
    |}]
;;

let matches signal k = signal ==:. to_int_repr k
