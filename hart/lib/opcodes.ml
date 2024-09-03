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

module Or_error = struct
  type t =
    | Error
    | Opcode of T.t
  [@@deriving compare, sexp, enumerate, variants]

  let encode t =
    match t with
    | Error -> 0
    | Opcode t -> 1 + T.Variants.to_rank t
  ;;

  let max_rank =
    List.map ~f:encode all |> List.max_elt ~compare:Int.compare |> Option.value_exn
  ;;

  let bits_to_repr = num_bits_to_represent max_rank

  let%expect_test "bits in decoded opcode" =
    print_s [%message (bits_to_repr : int)];
    [%expect {| (bits_to_repr 4) |}]
  ;;

  let decode signal =
    List.map
      ~f:(fun t ->
        mux2
          (matches signal t)
          (of_int ~width:bits_to_repr (encode (Opcode t)))
          (zero bits_to_repr))
      T.all
    |> List.fold ~init:(zero bits_to_repr) ~f:( |: )
  ;;
end

module Signals = struct
  type 'a t =
    { op : 'a
    ; op_imm : 'a
    ; jal : 'a
    ; jalr : 'a
    ; lui : 'a
    ; auipc : 'a
    ; branch : 'a
    ; load : 'a
    ; store : 'a
    ; fence : 'a
    ; system : 'a
    }
  [@@deriving hardcaml]

  let of_signal signal =
    let matches k = signal ==:. to_int_repr k in
    { op = matches Op
    ; op_imm = matches Op_imm
    ; jal = matches Jal
    ; jalr = matches Jalr
    ; lui = matches Lui
    ; auipc = matches Auipc
    ; branch = matches Branch
    ; load = matches Load
    ; store = matches Store
    ; fence = matches Fence
    ; system = matches System
    }
  ;;
end
