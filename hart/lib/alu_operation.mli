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

module Onehot : Onehot_intf.S with type base := t
