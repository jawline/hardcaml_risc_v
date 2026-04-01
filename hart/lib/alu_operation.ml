module T = struct
  type t =
    | Add
    | Sub
    | Sll
    | Slt
    | Xor
    | Sltu
    | Or
    | And
    | Srl
    | Sra
    | Sh1_add
    | Sh2_add
    | Sh3_add
  [@@deriving equal, enumerate, variants]
end

include T
module Onehot = Onehot.Make (T)
