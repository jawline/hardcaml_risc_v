open! Core
open Hardcaml
open Hardcaml_risc_v_hart
open! Bits

let assemble_i_type ~opcode ~funct3 ~rs1 ~rd ~immediate =
    concat_msb [ immediate; rs1; funct3; rd; opcode ]
  ;;

  let assemble_r_type ~opcode ~funct3 ~funct7 ~rs1 ~rs2 ~rd =
    concat_msb [ funct7; rs2; rs1; funct3; rd; opcode ]
  ;;

  let assemble_s_type ~opcode ~funct3 ~immediate ~rs1 ~rs2 =
    concat_msb [ sel_top immediate 7; rs2; rs1; funct3; sel_bottom immediate 5; opcode ]
  ;;

  let branch ~funct3 ~rs1 ~rs2 ~offset =
    assemble_s_type
      ~opcode:(Bits.of_int ~width:7 Opcodes.branch)
      ~funct3:(Bits.of_int ~width:3 (Funct3.Branch.to_int funct3))
      ~immediate:(Bits.of_int ~width:12 offset)
      ~rs1:(Bits.of_int ~width:5 rs1)
      ~rs2:(Bits.of_int ~width:5 rs2)
  ;;

  let op_imm ~funct3 ~rs1 ~rd ~immediate =
    assemble_i_type
      ~opcode:(Bits.of_int ~width:7 Opcodes.op_imm)
      ~funct3:(Bits.of_int ~width:3 (Funct3.Op.to_int funct3))
      ~rs1:(Bits.of_int ~width:5 rs1)
      ~rd:(Bits.of_int ~width:5 rd)
      ~immediate:(Bits.of_int ~width:12 immediate)
  ;;

  let op ~funct7 ~funct3 ~rs1 ~rs2 ~rd =
    assemble_r_type
      ~opcode:(Bits.of_int ~width:7 Opcodes.op)
      ~funct3:(Bits.of_int ~width:3 (Funct3.Op.to_int funct3))
      ~funct7:(Bits.of_int ~width:7 funct7)
      ~rs1:(Bits.of_int ~width:5 rs1)
      ~rs2:(Bits.of_int ~width:5 rs2)
      ~rd:(Bits.of_int ~width:5 rd)
  ;;

