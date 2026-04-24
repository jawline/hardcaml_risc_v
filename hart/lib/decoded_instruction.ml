open! Core
open Hardcaml
open Signal

module Make (Hart_config : Hart_config_intf.S) (Registers : Registers_intf.S) = struct
  let register_width = Register_width.bits Hart_config.register_width
  let select_register (registers : _ Registers.t) slot = mux slot registers.general

  let onehot_select_with_default ~default args =
    let all_valids = List.map ~f:(fun (arg : _ With_valid.t) -> arg.valid) args in
    let default_valid = ~:(List.reduce_exn ~f:( |: ) all_valids) in
    onehot_select ({ With_valid.valid = default_valid; value = default } :: args)
  ;;

  module Reg_indices = struct
    type 'a t =
      { rd : 'a [@bits register_width]
      ; rs1 : 'a [@bits register_width]
      ; rs2 : 'a [@bits register_width]
      }
    [@@deriving hardcaml]

    let decode ~registers scope instruction =
      let%hw rd_index = Instruction_parts.rd instruction in
      let%hw rs1_index = Instruction_parts.rs1 instruction in
      let%hw rs2_index = Instruction_parts.rs2 instruction in
      { rd = rd_index
      ; rs1 = select_register registers rs1_index
      ; rs2 = select_register registers rs2_index
      }
    ;;
  end

  module Immediates = struct
    type 'a t =
      { i_immediate : 'a
      ; s_immediate : 'a
      ; j_immediate : 'a
      ; u_immediate : 'a
      ; csr : 'a
      ; b_immediate : 'a
      }

    let decode scope instruction =
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
      let%hw b_immediate =
        Instruction_parts.b_immediate ~width:register_width instruction
      in
      { i_immediate; s_immediate; j_immediate; u_immediate; csr; b_immediate }
    ;;
  end

  module Opcodes = struct
    type 'a t =
      { is_op : 'a
      ; is_op_imm : 'a
      ; is_auipc : 'a
      ; is_lui : 'a
      ; is_jal : 'a
      ; is_jalr : 'a
      ; is_branch : 'a
      ; is_load : 'a
      ; is_store : 'a
      ; is_system : 'a
      ; is_ecall : 'a
      ; is_csr : 'a
      ; decoded : 'a Decoded_opcode.Packed.t
      }
    [@@deriving hardcaml]

    let decode scope instruction =
      let test_opcode op =
        Instruction_parts.opcode instruction ==:. Opcodes.to_int_repr op
      in
      let%hw funct3 = Instruction_parts.funct3 instruction in
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
      let%hw.Decoded_opcode.Packed.Of_signal decoded =
        Decoded_opcode.construct_onehot ~f:(function
          | ALU -> is_op |: is_op_imm |: is_lui |: is_auipc
          | Assign_pc_sum_of_arguments -> is_jal |: is_jalr
          | Branch -> is_branch
          | Load -> is_load
          | Store -> is_store
          | System -> is_system)
      in
      let%hw is_ecall = funct3 ==:. Funct3.System.to_int Funct3.System.Ecall_or_ebreak in
      let%hw is_csr =
        let f t = funct3 ==:. Funct3.System.to_int t in
        f Funct3.System.Csrrw |: f Funct3.System.Csrrs |: f Funct3.System.Csrrc
      in
      { is_op
      ; is_op_imm
      ; is_auipc
      ; is_lui
      ; is_jal
      ; is_jalr
      ; is_branch
      ; is_load
      ; is_store
      ; is_system
      ; is_ecall
      ; is_csr
      ; decoded
      }
    ;;
  end

  module Arguments = struct
    type 'a t =
      { argument_1 : 'a
      ; argument_2 : 'a
      ; argument_3 : 'a
      }

    let decode
          ~(opcodes : _ Opcodes.t)
          ~(immediates : _ Immediates.t)
          ~(reg_indices : _ Reg_indices.t)
          ~(registers : _ Registers.t)
          scope
      =
      let rs1 = reg_indices.rs1 in
      let rs2 = reg_indices.rs2 in
      let%hw argument_1 =
        onehot_select_with_default
          ~default:rs1
          [ { With_valid.valid = opcodes.is_jal |: opcodes.is_auipc
            ; value = registers.pc
            }
          ; { With_valid.valid = opcodes.is_lui; value = immediates.u_immediate }
          ]
      in
      let%hw argument_2 =
        onehot_select_with_default
          ~default:rs2
          [ { With_valid.valid = opcodes.is_op_imm |: opcodes.is_jalr
            ; value = immediates.i_immediate
            }
          ; { With_valid.valid = opcodes.is_jal; value = immediates.j_immediate }
          ; { With_valid.valid = opcodes.is_auipc; value = immediates.u_immediate }
          ; { With_valid.valid = opcodes.is_lui; value = zero register_width }
          ]
      in
      let%hw argument_3 =
        onehot_select
          [ { With_valid.valid = opcodes.is_branch; value = immediates.b_immediate }
          ; { With_valid.valid = opcodes.is_load; value = immediates.i_immediate }
          ; { With_valid.valid = opcodes.is_store; value = immediates.s_immediate }
          ]
      in
      { argument_1; argument_2; argument_3 }
    ;;
  end

  module T = struct
    type 'a t =
      { pc : 'a [@bits register_width]
      ; opcode : 'a Decoded_opcode.Packed.t
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
      ; muldiv_operation : 'a Funct3.Muldiv.Onehot.t option
            [@exists Hart_config.Extensions.zmul]
      ; is_muldiv : 'a option [@exists Hart_config.Extensions.zmul]
      ; branch_onehot : 'a Funct3.Branch.Onehot.t
      ; load_onehot : 'a Funct3.Load.Onehot.t
      ; store_onehot : 'a Funct3.Store.Onehot.t
      ; error : 'a
      }
    [@@deriving hardcaml ~rtlmangle:"$"]

    let decode_alu_operation instruction is_op_imm is_op is_lui is_auipc =
      let funct3 = Instruction_parts.funct3 instruction in
      let funct7 = Instruction_parts.funct7 instruction in
      let test_funct3 op = funct3 ==:. Funct3.Op.to_int op in
      let is_sh_add_funct7 = funct7 ==:. 0b0010000 in
      let funct7_switch = funct7.:(5) in
      let is_op_imm_or_not_funct7 = is_op_imm |: ~:is_sh_add_funct7 in
      let is_op_and_funct7_switch = is_op &: is_sh_add_funct7 in
      let op_mode =
        let if_zba v = if Hart_config.Extensions.zba then v else gnd in
        Alu_operation.Onehot.construct_onehot ~f:(fun op ->
          match op with
          | Alu_operation.Add -> test_funct3 Add_or_sub &: (is_op_imm |: ~:funct7_switch)
          | Sub -> test_funct3 Add_or_sub &: (~:is_op_imm &: funct7_switch)
          | Srl -> test_funct3 Srl_or_sra &: ~:funct7_switch
          | Sra -> test_funct3 Srl_or_sra &: funct7_switch
          | Sll -> test_funct3 Sll
          | Slt -> test_funct3 Slt_or_sh1add &: is_op_imm_or_not_funct7
          | Xor -> test_funct3 Xor_or_sh2add &: is_op_imm_or_not_funct7
          | Sltu -> test_funct3 Sltu
          | Or -> test_funct3 Or_or_sh3add &: is_op_imm_or_not_funct7
          | And -> test_funct3 And
          | Sh1_add -> if_zba (test_funct3 Slt_or_sh1add &: is_op_and_funct7_switch)
          | Sh2_add -> if_zba (test_funct3 Xor_or_sh2add &: is_op_and_funct7_switch)
          | Sh3_add -> if_zba (test_funct3 Or_or_sh3add &: is_op_and_funct7_switch))
      in
      let add =
        Alu_operation.Onehot.construct_onehot ~f:(function
          | Alu_operation.Add -> vdd
          | _ -> gnd)
      in
      Alu_operation.Onehot.Of_signal.mux2 (is_lui |: is_auipc) add op_mode
    ;;

    let of_instruction instruction registers scope =
      let rd_index = Instruction_parts.rd instruction in
      let%hw.Opcodes.Of_signal opcodes = Opcodes.decode scope instruction in
      let%hw.Reg_indices.Of_signal reg_indices =
        Reg_indices.decode ~registers scope instruction
      in
      let alu_operation =
        decode_alu_operation
          instruction
          opcodes.is_op_imm
          opcodes.is_op
          opcodes.is_lui
          opcodes.is_auipc
      in
      let immediates = Immediates.decode scope instruction in
      let decoded_arguments =
        Arguments.decode ~opcodes ~immediates ~registers ~reg_indices scope
      in
      let%hw funct3 = Instruction_parts.funct3 instruction in
      let%hw funct7 = Instruction_parts.funct7 instruction in
      let%hw error =
        opcodes.decoded.packed ==:. 0 |: (opcodes.is_op &: (funct7 &:. 0b1011_111 <>:. 0))
      in
      let muldiv_operation, is_muldiv =
        if Hart_config.Extensions.zmul
        then (
          let test_funct3 op = funct3 ==:. Funct3.Muldiv.to_int op in
          ( Some (Funct3.Muldiv.Onehot.construct_onehot ~f:test_funct3)
          , Some (funct7.:(0) &: opcodes.is_op) ))
        else None, None
      in
      let branch_onehot =
        let test_funct3 op = funct3 ==:. Funct3.Branch.to_int op in
        Funct3.Branch.Onehot.construct_onehot ~f:test_funct3
      in
      let load_onehot =
        let test_funct3 op = funct3 ==:. Funct3.Load.to_int op in
        Funct3.Load.Onehot.construct_onehot ~f:test_funct3
      in
      let store_onehot =
        let test_funct3 op = funct3 ==:. Funct3.Store.to_int op in
        Funct3.Store.Onehot.construct_onehot ~f:test_funct3
      in
      { pc = registers.pc
      ; opcode = opcodes.decoded
      ; funct3
      ; funct7
      ; argument_1 = decoded_arguments.argument_1
      ; argument_2 = decoded_arguments.argument_2
      ; argument_3 = decoded_arguments.argument_3
      ; rd =
          mux2
            (opcodes.is_system &: opcodes.is_ecall)
            (of_unsigned_int ~width:5 5)
            rd_index
      ; rd_value = reg_indices.rd
      ; csr = immediates.csr
      ; is_ecall = opcodes.is_ecall
      ; is_csr = opcodes.is_csr
      ; alu_operation
      ; muldiv_operation
      ; is_muldiv
      ; branch_onehot
      ; load_onehot
      ; store_onehot
      ; error
      }
    ;;
  end

  include T
  module With_valid = With_valid.Wrap.Make (T)
end
