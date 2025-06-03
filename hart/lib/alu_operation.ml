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
  [@@deriving equal, enumerate, variants]
end

include T
module Onehot = Onehot.Make (T)
