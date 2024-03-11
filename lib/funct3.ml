module Op_imm = struct
  let addi = 0b000
  let slli = 0b001
  let slti = 0b010
  let xori = 0b100
  let sltiu = 0b011
  let ori = 0b110
  let andi = 0b111

  (** Depending on the upper 7 bits of the imm this is either SRAI or SRLI *)
  let srli_or_srai = 0b101
end

module Op = struct
  (** These seem identical to the op_imm versions but I'll leave it for clarity in use. *)
  let add_or_sub = 0b000

  let sll = 0b001
  let slt = 0b010
  let xor = 0b100
  let sltu = 0b011
  let or_ = 0b110
  let and_ = 0b111

  (** Depending on the upper 7 bits of the imm this is either SRAI or SRLI *)
  let srl_or_sra = 0b101
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
