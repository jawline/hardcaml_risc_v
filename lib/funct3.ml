open Core

module Op_imm = struct
  type t =
    | Addi
    | Slli
    | Slti
    | Xori
    | Sltiu
    | Ori
    | Andi
    | (* Depending on the upper 7 bits of the imm this is either SRAI or SRLI *)
      Srli_or_srai

  let of_int_exn i =
    match i with
    | 0b000 -> Addi
    | 0b001 -> Slli
    | 0b010 -> Slti
    | 0b100 -> Xori
    | 0b011 -> Sltiu
    | 0b110 -> Ori
    | 0b111 -> Andi
    | 0b101 -> Srli_or_srai
    | _ -> raise_s [%message "BUG: Funct3 should be 3 bits wide"]
  ;;
end

module Op = struct
  type t =
    | (* These seem identical to the op_imm versions but I'll leave it for clarity in use. *)
      Add_or_sub
    | Sll
    | Slt
    | Xor
    | Sltu
    | Or
    | And
    | (* Depending on the upper 7 bits of the imm this is either SRAI or SRLI *)
      Srl_or_sra

  let of_int_exn i =
    match i with
    | 0b000 -> Add_or_sub
    | 0b001 -> Sll
    | 0b010 -> Slt
    | 0b100 -> Xor
    | 0b011 -> Sltu
    | 0b110 -> Or
    | 0b111 -> And
    | 0b101 -> Srl_or_sra
    | _ -> raise_s [%message "BUG: Funct3 should be 3 bits wide"]
  ;;
end

module Branch = struct
  let beq = 0b000
  let bne = 0b001
  let blt = 0b100
  let bge = 0b101
  let bltu = 0b110
  let bgeu = 0b111
end

module Load = struct
  let lb = 0b000
  let lh = 0b001
  let lw = 0b010
  let lbu = 0b100
  let lhu = 0b101
end

module Store = struct
  let sb = 0b000
  let sh = 0b001
  let sw = 0b010
end

module System = struct
  (** If the last 12 bits are 0 then this is ECALL otherwise it is EBREAK *)
  let ecall_or_ebreak = 0b000

  let csrrw = 0b001
  let csrrs = 0b010
  let csrrc = 0b011
  let csrrwi = 0b101
  let csrrsi = 0b110
  let csrrci = 0b111
end
