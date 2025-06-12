open! Core
open Hardcaml
open Signal

module Make (Hart_config : Hart_config_intf.S) (Registers : Registers_intf.S) = struct
  let register_width = Register_width.bits Hart_config.register_width

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
    ; alu_operation : 'a Alu_operation.Onehot.t
    ; branch_onehot : 'a Funct3.Branch.Onehot.t
    ; load_onehot : 'a Funct3.Load.Onehot.t
    ; store_onehot : 'a Funct3.Store.Onehot.t
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
    (* Immediate decodings *)
    let%hw i_immediate =
      Instruction_parts.i_immediate ~width:register_width instruction
    in
    let%hw s_immediate =
      Instruction_parts.s_immediate ~width:register_width instruction
    in
    let%hw j_immediate =
      Instruction_parts.j_immediate ~width:register_width instruction
    in
    let%hw u_immediate =
      Instruction_parts.u_immediate ~width:register_width instruction
    in
    let%hw csr = Instruction_parts.csr ~width:12 instruction in
    (* Register decodings *)
    let%hw rd_index = Instruction_parts.rd instruction in
    let%hw rs1_index = Instruction_parts.rs1 instruction in
    let%hw rs2_index = Instruction_parts.rs2 instruction in
    let%hw rs1 = select_register registers rs1_index in
    let%hw rs2 = select_register registers rs2_index in
    (* Opcode decodings *)
    let test_opcode op =
      Instruction_parts.opcode instruction ==:. Opcodes.to_int_repr op
    in
    let%hw is_op = test_opcode Op in
    let%hw is_op_imm = test_opcode Op_imm in
    let%hw is_auipc = test_opcode Auipc in
    let%hw is_lui = test_opcode Lui in
    let%hw is_jal = test_opcode Jal in
    let%hw is_jalr = test_opcode Jalr in
    let%hw is_branch = test_opcode Branch in
    let%hw is_load = test_opcode Load in
    let%hw is_store = test_opcode Store in
    let%hw is_system = test_opcode System in
    let decoded_opcode =
      Decoded_opcode.construct_onehot ~f:(function
        | ALU ->
          test_opcode Op |: test_opcode Op_imm |: test_opcode Lui |: test_opcode Auipc
        | Assign_pc_sum_of_arguments -> test_opcode Jal |: test_opcode Jalr
        | Branch -> test_opcode Branch
        | Load -> test_opcode Load
        | Store -> test_opcode Store
        | System -> test_opcode System)
    in
    (* Function Decodings *)
    let%hw funct3 = Instruction_parts.funct3 instruction in
    let%hw is_ecall = funct3 ==:. Funct3.System.to_int Funct3.System.Ecall_or_ebreak in
    let%hw is_csr =
      let f t = funct3 ==:. Funct3.System.to_int t in
      f Funct3.System.Csrrw |: f Funct3.System.Csrrs |: f Funct3.System.Csrrc
    in
    let funct7 = Instruction_parts.funct7 instruction in
    let funct7_switch = funct7.:(5) in
    let funct7_bit_other_than_switch_is_selected = funct7 &:. 0b1011_111 <>:. 0 in
    let%hw.Alu_operation.Onehot.Of_signal decoded_alu_operation =
      (* The ALU unit is used to implement several instructions including
         Op, Op_imm, Lui and Auipc. For Op and Op_imm the operation is
         decided by funct3. For other instructions the ALU always acts as
         an add. *)
      let test_funct3 op = funct3 ==:. Funct3.Op.to_int op in
      let op_mode =
        Alu_operation.Onehot.construct_onehot ~f:(fun op ->
          match op with
          | Alu_operation.Add ->
            (* The operation is an Add if Op &: ~:funct7 or if Op_imm. Op_imm
             does not have sub and uses the funct switch to encode it's
             immediate. *)
            test_funct3 Add_or_sub &: (is_op_imm |: ~:funct7_switch)
          | Sub ->
            (* Sub is if Add_or_sub &: ~:Add. *)
            test_funct3 Add_or_sub &: (~:is_op_imm &: funct7_switch)
          | Srl -> test_funct3 Srl_or_sra &: ~:funct7_switch
          | Sra -> test_funct3 Srl_or_sra &: funct7_switch
          | Sll -> test_funct3 Sll
          | Slt -> test_funct3 Slt
          | Xor -> test_funct3 Xor
          | Sltu -> test_funct3 Sltu
          | Or -> test_funct3 Or
          | And -> test_funct3 And)
      in
      let add =
        Alu_operation.Onehot.construct_onehot ~f:(function
          | Alu_operation.Add -> vdd
          | _ -> gnd)
      in
      Alu_operation.Onehot.Of_signal.mux2 (is_lui |: is_auipc) add op_mode
    in
    let%hw argument_1 =
      onehot_select_with_default
        ~default:rs1
        [ { With_valid.valid = is_jal |: is_auipc; value = registers.pc }
        ; { With_valid.valid = is_lui; value = u_immediate }
        ]
    in
    let%hw argument_2 =
      onehot_select_with_default
        ~default:rs2
        [ { With_valid.valid = is_op_imm |: is_jalr; value = i_immediate }
        ; { With_valid.valid = is_jal; value = j_immediate }
        ; { With_valid.valid = is_auipc; value = u_immediate }
        ; { With_valid.valid = is_lui; value = zero register_width }
        ]
    in
    let%hw argument_3 =
      onehot_select
        [ { With_valid.valid = is_branch
          ; value = Instruction_parts.b_immediate ~width:register_width instruction
          }
        ; { With_valid.valid = is_load; value = i_immediate }
        ; { With_valid.valid = is_store; value = s_immediate }
        ]
    in
    { opcode = decoded_opcode
    ; funct3
    ; funct7
    ; argument_1
    ; argument_2
    ; argument_3
    ; rd = mux2 (is_system &: is_ecall) (of_unsigned_int ~width:5 5) rd_index
    ; rd_value = select_register registers (Instruction_parts.rd instruction)
    ; csr
    ; is_ecall
    ; is_csr
    ; alu_operation = decoded_alu_operation
    ; branch_onehot =
        (let test_funct3 op = funct3 ==:. Funct3.Branch.to_int op in
         Funct3.Branch.Onehot.construct_onehot ~f:test_funct3)
    ; load_onehot =
        (let test_funct3 op = funct3 ==:. Funct3.Load.to_int op in
         Funct3.Load.Onehot.construct_onehot ~f:test_funct3)
    ; store_onehot =
        (let test_funct3 op = funct3 ==:. Funct3.Store.to_int op in
         Funct3.Store.Onehot.construct_onehot ~f:test_funct3)
    ; error =
        decoded_opcode.packed ==:. 0 |: (is_op &: funct7_bit_other_than_switch_is_selected)
    }
  ;;
end
