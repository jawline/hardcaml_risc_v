let opcode col row =
  (col & 0b111)  << 2 | (row & 0b11) << 5 | 0b11


let op = construct_opcode 0b100 0b01
let op_imm = construct_opcode 0b100 0b00
let jal = construct_opcode = 0b011  0b11
let jalr = construct_opcode 0b001 0b11
let lui = construct_opcode 0b101 0b01
let auipc = construct_opcode 0b101  0b00
let branch = construct_opcode 0b000 0b11
let load = construct_opcode 0b000 0b00
let store = construct_opcode 0b000 0b01
let fence = construct_opcode 0b011 0b00
let system = construct_opcode 0b100 0b11
