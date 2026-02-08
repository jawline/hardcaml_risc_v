open! Core

module Op = struct
  module T = struct
    type t =
      | (* These seem identical to the op_imm versions but I'll leave it for
         clarity in use. *)
        Add_or_sub
      | Sll
      | Slt
      | Xor
      | Sltu
      | Or
      | And
      | (* Depending on the upper 7 bits of the imm this is either SRAI or SRLI *)
        Srl_or_sra
    [@@deriving equal, enumerate, variants]

    let to_int t =
      match t with
      | Add_or_sub -> 0b000
      | Sll -> 0b001
      | Slt -> 0b010
      | Xor -> 0b100
      | Sltu -> 0b011
      | Or -> 0b110
      | And -> 0b111
      | Srl_or_sra -> 0b101
    ;;

    let of_int i =
      match i with
      | 0b000 -> Some Add_or_sub
      | 0b001 -> Some Sll
      | 0b010 -> Some Slt
      | 0b100 -> Some Xor
      | 0b011 -> Some Sltu
      | 0b110 -> Some Or
      | 0b111 -> Some And
      | 0b101 -> Some Srl_or_sra
      | _ -> None
    ;;
  end

  include T
  module Onehot = Onehot.Make (T)
end

module Muldiv = struct
  module T = struct
    type t =
      | Mul
      | MulHigh
      | MulHigh_Signed_Unsigned
      | MulHigh_Unsigned
    [@@deriving equal, enumerate, variants]

    let to_int t =
      match t with
      | Mul -> 0b000
      | MulHigh -> 0b001
      | MulHigh_Signed_Unsigned -> 0b010
      | MulHigh_Unsigned -> 0b011
    ;;

    let of_int i =
      match i with
      | 0b000 -> Some Mul
      | 0b001 -> Some MulHigh
      | 0b010 -> Some MulHigh_Signed_Unsigned
      | 0b011 -> Some MulHigh_Unsigned
      | _ -> None
    ;;
  end

  include T
  module Onehot = Onehot.Make (T)
end

module Branch = struct
  module T = struct
    type t =
      | Beq
      | Bne
      | Blt
      | Bge
      | Bltu
      | Bgeu
    [@@deriving equal, enumerate, variants]

    let to_int t =
      match t with
      | Beq -> 0b000
      | Bne -> 0b001
      | Blt -> 0b100
      | Bge -> 0b101
      | Bltu -> 0b110
      | Bgeu -> 0b111
    ;;

    let of_int i =
      match i with
      | 0b000 -> Some Beq
      | 0b001 -> Some Bne
      | 0b100 -> Some Blt
      | 0b101 -> Some Bge
      | 0b110 -> Some Bltu
      | 0b111 -> Some Bgeu
      | _ -> None
    ;;
  end

  include T
  module Onehot = Onehot.Make (T)
end

module Load = struct
  module T = struct
    type t =
      | Lb
      | Lh
      | Lw
      | Lbu
      | Lhu
    [@@deriving enumerate, variants]

    let to_int = function
      | Lb -> 0b000
      | Lh -> 0b001
      | Lw -> 0b010
      | Lbu -> 0b100
      | Lhu -> 0b101
    ;;

    let of_int = function
      | 0b000 -> Some Lb
      | 0b001 -> Some Lh
      | 0b010 -> Some Lw
      | 0b100 -> Some Lbu
      | 0b101 -> Some Lhu
      | _ -> None
    ;;
  end

  include T
  module Onehot = Onehot.Make (T)
end

module Store = struct
  module T = struct
    type t =
      | Sb
      | Sh
      | Sw
    [@@deriving enumerate, variants]

    let to_int = function
      | Sb -> 0b000
      | Sh -> 0b001
      | Sw -> 0b010
    ;;

    let of_int = function
      | 0b000 -> Some Sb
      | 0b001 -> Some Sh
      | 0b010 -> Some Sw
      | _ -> None
    ;;
  end

  include T
  module Onehot = Onehot.Make (T)
end

module System = struct
  type t =
    | (* If the last 12 bits are 0 then this is ECALL otherwise it is EBREAK *)
      Ecall_or_ebreak
    | Csrrw
    | Csrrs
    | Csrrc
    | Csrrwi
    | Csrrsi
    | Csrrci
  [@@deriving enumerate]

  let to_int = function
    | Ecall_or_ebreak -> 0
    | Csrrw -> 0b001
    | Csrrs -> 0b010
    | Csrrc -> 0b011
    | Csrrwi -> 0b101
    | Csrrsi -> 0b110
    | Csrrci -> 0b111
  ;;
end
