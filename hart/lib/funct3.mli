open! Core

module Op : sig
  type t =
    | (* These seem identical to the op_imm versions but I'll leave it for
         clarity in use. *)
      Add_or_sub
    | Sll
    | Slt_or_sh1add (* Switch on upper 7 bits *)
    | Xor_or_sh2add (* Switch on upper 7 bits *)
    | Sltu
    | Or_or_sh3add (* Switch on upper 7 bits *)
    | And
    | (* Depending on the upper 7 bits of the imm this is either SRAI or SRLI *)
      Srl_or_sra
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
