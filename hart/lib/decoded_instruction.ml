open! Core
open Hardcaml
open Signal

module Make (Hart_config : Hart_config_intf.S) (Registers : Registers_intf.S) = struct
  let register_width = Register_width.bits Hart_config.register_width

  type 'a t =
    { opcode : 'a [@bits 7]
    ; funct3 : 'a [@bits 3]
    ; funct7 : 'a [@bits 7]
    ; rs1 : 'a [@bits register_width]
    ; rs2 : 'a [@bits register_width]
    (** Rd just points to the rd slot rather than containing the register *)
    ; rd : 'a [@bits 5]
    ; rd_value : 'a [@bits register_width]
    ; i_immediate : 'a [@bits register_width]
    ; j_immediate : 'a [@bits register_width]
    ; s_immediate : 'a [@bits register_width]
    ; u_immediate : 'a [@bits register_width]
    ; b_immediate : 'a [@bits register_width]
    ; load_address : 'a [@bits register_width]
    ; store_address : 'a [@bits register_width]
    ; funct7_switch : 'a
    ; funct7_bit_other_than_switch_is_selected : 'a
    ; is_ecall : 'a
    ; decoded_opcode_or_error : 'a [@bits Opcodes.Or_error.bits_to_repr]
    ; opcode_signals : 'a Opcodes.Signals.t
    }
  [@@deriving sexp_of, hardcaml ~rtlmangle:"$"]

  let select_register (registers : _ Registers.t) slot = mux slot registers.general

  let of_instruction instruction registers _scope =
    let decoded_opcode_or_error = Opcodes.Or_error.decode (Decoder.opcode instruction) in
    let opcode_signals = Opcodes.Signals.of_signal (Decoder.opcode instruction) in
    let is_ecall =
      let system = opcode_signals.system in
      let ecall =
        Decoder.funct3 instruction ==:. Funct3.System.to_int Funct3.System.Ecall_or_ebreak
      in
      system &: ecall
    in
    let rs1 = select_register registers (Decoder.rs1 instruction) in
    let rs2 = select_register registers (Decoder.rs2 instruction) in
    let i_immediate = Decoder.i_immediate ~width:register_width instruction in
    let s_immediate = Decoder.s_immediate ~width:register_width instruction in
    let funct7 = Decoder.funct7 instruction in
    { opcode = Decoder.opcode instruction
    ; funct3 = Decoder.funct3 instruction
    ; funct7
    ; rs1
    ; rs2
    ; rd = mux2 is_ecall (of_int ~width:5 5) (Decoder.rd instruction)
    ; rd_value = select_register registers (Decoder.rd instruction)
    ; i_immediate
    ; s_immediate
    ; j_immediate = Decoder.j_immediate ~width:register_width instruction
    ; u_immediate = Decoder.u_immediate ~width:register_width instruction
    ; b_immediate = Decoder.b_immediate ~width:register_width instruction
    ; load_address = rs1 +: i_immediate
    ; store_address = rs1 +: s_immediate
    ; funct7_switch = funct7.:(5)
    ; funct7_bit_other_than_switch_is_selected = funct7 &:. 0b1011_111 <>:. 0
    ; is_ecall
    ; decoded_opcode_or_error
    ; opcode_signals
    }
  ;;
end
