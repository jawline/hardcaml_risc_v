open! Core
open Hardcaml
open Signal

module Make (Hart_config : Hart_config_intf.S) (Registers : Registers_intf.S) = struct
  let register_width = Register_width.bits Hart_config.register_width

  module ALU_specifics = struct
    type 'a t =
      { subtract_instead_of_add : 'a
      ; arithmetic_shift : 'a
      }
    [@@deriving hardcaml ~rtlmangle:"$"]
  end

  type 'a t =
    { opcode : 'a Decoded_opcode.Packed.t
    ; funct3 : 'a [@bits 3]
    ; funct7 : 'a [@bits 7]
    ; rs1 : 'a [@bits register_width]
    ; rs2 : 'a [@bits register_width]
      (** Rd just points to the rd slot rather than containing the register *)
    ; rd : 'a [@bits 5]
    ; rd_value : 'a [@bits register_width]
    ; csr : 'a [@bits 12]
    ; i_immediate : 'a [@bits register_width]
    ; j_immediate : 'a [@bits register_width]
    ; s_immediate : 'a [@bits register_width]
    ; u_immediate : 'a [@bits register_width]
    ; b_immediate : 'a [@bits register_width]
    ; load_address : 'a [@bits register_width]
    ; store_address : 'a [@bits register_width]
    ; is_ecall : 'a
    ; is_csr : 'a
    ; alu_specifics : 'a ALU_specifics.t
    ; error : 'a
    }
  [@@deriving hardcaml ~rtlmangle:"$"]

  let select_register (registers : _ Registers.t) slot = mux slot registers.general

  let of_instruction instruction registers scope =
    let%hw funct3 = Decoder.funct3 instruction in
    let%hw is_ecall = funct3 ==:. Funct3.System.to_int Funct3.System.Ecall_or_ebreak in
    let%hw is_csr =
      let f t = funct3 ==:. Funct3.System.to_int t in
      f Funct3.System.Csrrw |: f Funct3.System.Csrrs |: f Funct3.System.Csrrc
    in
    let rs1 = select_register registers (Decoder.rs1 instruction) in
    let rs2 = select_register registers (Decoder.rs2 instruction) in
    let i_immediate = Decoder.i_immediate ~width:register_width instruction in
    let s_immediate = Decoder.s_immediate ~width:register_width instruction in
    let funct7 = Decoder.funct7 instruction in
    let csr = Decoder.csr ~width:12 instruction in
    let test_opcode op = Decoder.opcode instruction ==:. Opcodes.to_int_repr op in
    let is_op = test_opcode Op in
    let decoded_opcode =
      Decoded_opcode.construct_onehot ~f:(function
        | ALU -> test_opcode Op |: test_opcode Op_imm
        | Jal -> test_opcode Jal
        | Jalr -> test_opcode Jalr
        | Lui -> test_opcode Lui
        | Auipc -> test_opcode Auipc
        | Branch -> test_opcode Branch
        | Load -> test_opcode Load
        | Store -> test_opcode Store
        | Fence -> test_opcode Fence
        | System -> test_opcode System)
    in
    let funct7_switch = funct7.:(5) in
    let funct7_bit_other_than_switch_is_selected = funct7 &:. 0b1011_111 <>:. 0 in
    { opcode = decoded_opcode
    ; funct3
    ; funct7
    ; rs1
    ; rs2 = mux2 (test_opcode Op_imm) i_immediate rs2
    ; rd =
        mux2
          (Decoded_opcode.valid decoded_opcode System &: is_ecall)
          (of_unsigned_int ~width:5 5)
          (Decoder.rd instruction)
    ; rd_value = select_register registers (Decoder.rd instruction)
    ; csr
    ; i_immediate
    ; s_immediate
    ; j_immediate = Decoder.j_immediate ~width:register_width instruction
    ; u_immediate = Decoder.u_immediate ~width:register_width instruction
    ; b_immediate = Decoder.b_immediate ~width:register_width instruction
    ; load_address = rs1 +: i_immediate
    ; store_address = rs1 +: s_immediate
    ; is_ecall
    ; is_csr
    ; alu_specifics =
        { ALU_specifics.subtract_instead_of_add = funct7_switch &: is_op
        ; arithmetic_shift = funct7_switch
        }
    ; error =
        decoded_opcode.packed ==:. 0 |: (is_op &: funct7_bit_other_than_switch_is_selected)
    }
  ;;
end
