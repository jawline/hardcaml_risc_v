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
    ; i_immediate : 'a [@bits register_width]
    ; j_immediate : 'a [@bits register_width]
    ; u_immediate : 'a [@bits register_width]
    }
  [@@deriving sexp_of, hardcaml]

  let select_register (registers : _ Registers.t) slot = mux slot registers.general

  let of_instruction instruction registers =
    { opcode = Decoder.opcode instruction
    ; funct3 = Decoder.funct3 instruction
    ; funct7 = Decoder.funct7 instruction
    ; rs1 = select_register registers (Decoder.rs1 instruction)
    ; rs2 = select_register registers (Decoder.rs2 instruction)
    ; rd = Decoder.rd instruction
    ; i_immediate = Decoder.i_immediate ~width:register_width instruction
    ; j_immediate = Decoder.j_immediate ~width:register_width instruction
    ; u_immediate = Decoder.u_immediate ~width:register_width instruction
    }
  ;;
end
