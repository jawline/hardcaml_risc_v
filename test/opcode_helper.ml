open! Core
open Hardcaml
open Hardcaml_risc_v_hart
open! Bits

let assemble_i_type ~opcode ~funct3 ~rs1 ~rd ~immediate =
  let opcode = Opcodes.to_int_repr opcode |> Bits.of_int ~width:7 in
  concat_msb [ immediate; rs1; funct3; rd; opcode ]
;;

let assemble_r_type ~opcode ~funct3 ~funct7 ~rs1 ~rs2 ~rd =
  let opcode = Opcodes.to_int_repr opcode |> Bits.of_int ~width:7 in
  concat_msb [ funct7; rs2; rs1; funct3; rd; opcode ]
;;

let assemble_s_type ~opcode ~funct3 ~immediate ~rs1 ~rs2 =
  let opcode = Opcodes.to_int_repr opcode |> Bits.of_int ~width:7 in
  concat_msb
    [ sel_top ~width:7 immediate
    ; rs2
    ; rs1
    ; funct3
    ; sel_bottom ~width:5 immediate
    ; opcode
    ]
;;

let assemble_b_type ~opcode ~funct3 ~immediate ~rs1 ~rs2 =
  let opcode = Opcodes.to_int_repr opcode |> Bits.of_int ~width:7 in
  concat_msb
    [ immediate.:(12)
    ; immediate.:[10, 5]
    ; rs2
    ; rs1
    ; funct3
    ; immediate.:[4, 1]
    ; immediate.:(11)
    ; opcode
    ]
;;

let assemble_j_type ~opcode ~rd ~immediate =
  let opcode = Opcodes.to_int_repr opcode |> Bits.of_int ~width:7 in
  concat_msb
    [ immediate.:(19)
    ; immediate.:[10, 1]
    ; immediate.:(11)
    ; immediate.:[19, 12]
    ; rd
    ; opcode
    ]
;;

let load ~funct3 ~rs1 ~rd ~immediate =
  assemble_i_type
    ~opcode:Opcodes.Load
    ~funct3:(Bits.of_int ~width:3 (Funct3.Load.to_int funct3))
    ~rs1:(Bits.of_int ~width:5 rs1)
    ~rd:(Bits.of_int ~width:5 rd)
    ~immediate:(Bits.of_int ~width:12 immediate)
;;

let store ~funct3 ~rs1 ~rs2 ~immediate =
  assemble_s_type
    ~opcode:Store
    ~funct3:(Bits.of_int ~width:3 (Funct3.Store.to_int funct3))
    ~rs1:(Bits.of_int ~width:5 rs1)
    ~rs2:(Bits.of_int ~width:5 rs2)
    ~immediate:(Bits.of_int ~width:12 immediate)
;;

let jal ~rd ~offset =
  assemble_j_type
    ~opcode:Jal
    ~rd:(Bits.of_int ~width:5 rd)
    ~immediate:(Bits.of_int ~width:20 offset)
;;

let jalr ~rd ~rs1 ~offset =
  assemble_i_type
    ~opcode:Jalr
    ~funct3:(Bits.of_int ~width:3 0)
    ~rs1:(Bits.of_int ~width:5 rs1)
    ~rd:(Bits.of_int ~width:5 rd)
    ~immediate:(Bits.of_int ~width:12 offset)
;;

let branch ~funct3 ~rs1 ~rs2 ~offset =
  assemble_b_type
    ~opcode:Branch
    ~funct3:(Bits.of_int ~width:3 (Funct3.Branch.to_int funct3))
    ~immediate:(Bits.of_int ~width:13 offset)
    ~rs1:(Bits.of_int ~width:5 rs1)
    ~rs2:(Bits.of_int ~width:5 rs2)
;;

let op_imm ~funct3 ~rs1 ~rd ~immediate =
  assemble_i_type
    ~opcode:Op_imm
    ~funct3:(Bits.of_int ~width:3 (Funct3.Op.to_int funct3))
    ~rs1:(Bits.of_int ~width:5 rs1)
    ~rd:(Bits.of_int ~width:5 rd)
    ~immediate:(Bits.of_int ~width:12 immediate)
;;

let op ~funct7 ~funct3 ~rs1 ~rs2 ~rd =
  assemble_r_type
    ~opcode:Op
    ~funct3:(Bits.of_int ~width:3 (Funct3.Op.to_int funct3))
    ~funct7:(Bits.of_int ~width:7 funct7)
    ~rs1:(Bits.of_int ~width:5 rs1)
    ~rs2:(Bits.of_int ~width:5 rs2)
    ~rd:(Bits.of_int ~width:5 rd)
;;

let ecall =
  assemble_i_type
    ~opcode:System
    ~funct3:(Bits.of_int ~width:3 (Funct3.System.to_int Funct3.System.Ecall_or_ebreak))
    ~rs1:(Bits.of_int ~width:5 0)
    ~rd:(Bits.of_int ~width:5 0)
    ~immediate:(Bits.of_int ~width:12 0)
;;

let instructions_to_data instructions =
  Bits.concat_lsb instructions
  |> Bits.split_lsb ~part_width:8
  |> List.map ~f:Bits.to_char
  |> String.of_char_list
;;

let hello_world_program =
  let print_string = "Hello world!" in
  instructions_to_data
    [ op_imm ~funct3:Funct3.Op.Add_or_sub ~rs1:0 ~rd:5 ~immediate:0
    ; op_imm ~funct3:Funct3.Op.Add_or_sub ~rs1:0 ~rd:6 ~immediate:16
    ; op_imm
        ~funct3:Funct3.Op.Add_or_sub
        ~rs1:0
        ~rd:7
        ~immediate:(String.length print_string)
    ; ecall
    ]
  ^ print_string
;;

let clear_packet =
  let packet_len_parts =
    Bits.of_int ~width:16 2 |> split_msb ~part_width:8 |> List.map ~f:Bits.to_int
  in
  [ Char.to_int 'Q' ]
  @ packet_len_parts
  (* This 0 is the router tag. 0 will
     route to DMA and 1 will route to clear. A one tag indicates a
     clear signal. *) @ [ 1; 0 ]
;;

let dma_packet ~address packet =
  (* We add the header and then the packet length before the packet *)
  let packet = String.to_list packet in
  let packet_len_parts =
    Bits.of_int ~width:16 (List.length packet + 5)
    |> split_msb ~part_width:8
    |> List.map ~f:Bits.to_int
  in
  let address =
    Bits.of_int ~width:32 address |> split_msb ~part_width:8 |> List.map ~f:Bits.to_int
  in
  [ Char.to_int 'Q' ]
  @ packet_len_parts
  (* This 0 is the router tag. 0 will
     route to DMA and 1 will route to clear. *) @ [ 0 ]
  @ address
  @ List.map ~f:Char.to_int packet
;;
