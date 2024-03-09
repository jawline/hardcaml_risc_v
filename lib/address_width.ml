type t =
  | RV32
  | RV64

let in_bits =
  match t with
  | RV32 -> 32
  | RV64 -> 64
;;
