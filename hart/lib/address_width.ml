type t =
  | RV32
  | RV64

let bits t =
  match t with
  | RV32 -> 32
  | RV64 -> 64
;;
