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
    ; argument_1 : 'a [@bits register_width]
    ; argument_2 : 'a [@bits register_width]
    ; argument_3 : 'a [@bits register_width]
    ; rd : 'a [@bits 5]
      (** Rd just points to the rd slot rather than containing the register *)
    ; rd_value : 'a [@bits register_width]
    ; csr : 'a [@bits 12]
    ; is_ecall : 'a
    ; is_csr : 'a
    ; alu_specifics : 'a ALU_specifics.t
    ; error : 'a
    }
  [@@deriving hardcaml ~rtlmangle:"$"]

  let select_register (registers : _ Registers.t) slot = mux slot registers.general

  let onehot_select_with_default ~default args =
    let all_valids = List.map ~f:(fun (arg : _ With_valid.t) -> arg.valid) args in
    (* Default if no value is set. *)
    let default_valid = ~:(List.reduce_exn ~f:( |: ) all_valids) in
    onehot_select ({ With_valid.valid = default_valid; value = default } :: args)
  ;;

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
    let j_immediate = Decoder.j_immediate ~width:register_width instruction in
    let load_address = rs1 +: i_immediate in
    let store_address = rs1 +: s_immediate in
    let u_immediate = Decoder.u_immediate ~width:register_width instruction in
    { opcode = decoded_opcode
    ; funct3
    ; funct7
    ; argument_1 =
        onehot_select_with_default
          ~default:rs1
          [ { With_valid.valid = test_opcode Jal; value = j_immediate }
          ; { With_valid.valid = test_opcode Load; value = load_address }
          ; { With_valid.valid = test_opcode Store; value = store_address }
          ; { With_valid.valid = test_opcode Lui |: test_opcode Auipc
            ; value = u_immediate
            }
          ]
    ; argument_2 =
        onehot_select_with_default
          ~default:rs2
          [ { With_valid.valid = test_opcode Op_imm |: test_opcode Jalr
            ; value = i_immediate
            }
          ]
    ; argument_3 = Decoder.b_immediate ~width:register_width instruction
    ; rd =
        mux2
          (Decoded_opcode.valid decoded_opcode System &: is_ecall)
          (of_unsigned_int ~width:5 5)
          (Decoder.rd instruction)
    ; rd_value = select_register registers (Decoder.rd instruction)
    ; csr
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
