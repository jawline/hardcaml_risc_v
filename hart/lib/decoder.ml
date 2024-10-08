open Core
open Hardcaml
open Signal

let opcode t = t.:[6, 0]
let rd t = t.:[11, 7]
let rs1 t = t.:[19, 15]
let rs2 t = t.:[24, 20]
let funct3 t = t.:[14, 12]
let funct7 t = t.:[31, 25]

(** Sign extend an immediate to ~to_ bits, filling the msbs with all ones or
    all zeros depending on the sign bit of the original value. *)
let sign_extend ~width t =
  let sign_bit = msb t in
  let extended_bits = width - Signal.width t in
  concat_msb [ mux2 sign_bit (ones extended_bits) (zero extended_bits); t ]
;;

(** u-immediates are 20-bit immediates encoded at the top of the instruction.
    When decoded, the 20 bits fill the upper 20 bits of the register and the
    remaining bits are filled with zeros. *)
let u_immediate ~width t = concat_msb [ sel_top ~width:20 t; zero (width - 20) ]

(** Sign extended 12-bit immediate stored in the upper 12 bits of the
    instruction. *)
let i_immediate ~width t = sel_top ~width:12 t |> sign_extend ~width

(** 12-bit unsigned immediate in the upper 12 bits of the instruction. *)
let csr ~width t = uresize ~width (sel_top ~width:12 t)

(** S-type immediates are sign extended 12 bit immediates packed into bits
    (7-11) for imm(0:4) and bits 25-31 for bits 5-11 (inclusive). *)
let s_immediate ~width t = concat_msb [ t.:[31, 25]; t.:[11, 7] ] |> sign_extend ~width

(** B-type immediates are sign extended 13 bit immediates packed into bits
    (8-11) for imm(1:4), bits 25-30 for imm(5:10), bit 7 for imm(11) and bit 31
    for imm(12). imm(0) is always 0 (inclusive). *)
let b_immediate ~width t =
  concat_msb [ t.:(31); t.:(7); t.:[30, 25]; t.:[11, 8]; gnd ] |> sign_extend ~width
;;

(** J-type the upper 20 bits of a 21-bit value (sign extended) in the order
    bits 21-30 => imm(1:10), bit 20 => imm(11), bits 12-19 => imm(12:19), bits
    31 => imm(20) (The sign extend bit). *)
let j_immediate ~width t =
  concat_msb [ t.:(31); t.:[19, 12]; t.:(20); t.:[30, 21]; gnd ] |> sign_extend ~width
;;
