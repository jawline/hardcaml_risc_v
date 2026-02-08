open! Core

module Op : sig
  type t =
    | Add_or_sub
    (** Depending on the funct7 switch bit this is either add
        or sub. For immediate it is always add. *)
    | Sll
    | Slt
    | Xor
    | Sltu
    | Or
    | And
    | Srl_or_sra (** Depending on the funct7 switch bit this is either srl or sra *)
  [@@deriving equal, enumerate, variants]

  val to_int : t -> int
  val of_int : int -> t option

  module Onehot : Onehot_intf.S with type base := t
end

module Muldiv : sig
  type t =
    | Mul
    | MulHigh
    | MulHigh_Signed_Unsigned
    | MulHigh_Unsigned
  [@@deriving equal, enumerate, variants]

  val to_int : t -> int
  val of_int : int -> t option

  module Onehot : Onehot_intf.S with type base := t
end

module Branch : sig
  type t =
    | Beq
    | Bne
    | Blt
    | Bge
    | Bltu
    | Bgeu
  [@@deriving equal, enumerate, variants]

  val to_int : t -> int
  val of_int : int -> t option

  module Onehot : Onehot_intf.S with type base := t
end

module Load : sig
  type t =
    | Lb
    | Lh
    | Lw
    | Lbu
    | Lhu
  [@@deriving enumerate]

  val to_int : t -> int
  val of_int : int -> t option

  module Onehot : Onehot_intf.S with type base := t
end

module Store : sig
  type t =
    | Sb
    | Sh
    | Sw
  [@@deriving enumerate]

  val to_int : t -> int
  val of_int : int -> t option

  module Onehot : Onehot_intf.S with type base := t
end

module System : sig
  type t =
    | Ecall_or_ebreak
    (** If the last 12 bits are 0 then this is ECALL otherwise it is EBREAK *)
    | Csrrw
    | Csrrs
    | Csrrc
    | Csrrwi
    | Csrrsi
    | Csrrci
  [@@deriving enumerate]

  val to_int : t -> int
end
