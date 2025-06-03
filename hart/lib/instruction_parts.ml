open Core
open Hardcaml
open Signal

let opcode t = t.:[6, 0]
let rd t = t.:[11, 7]
let rs1 t = t.:[19, 15]
let rs2 t = t.:[24, 20]
let funct3 t = t.:[14, 12]
let funct7 t = t.:[31, 25]
let u_immediate ~width t = concat_msb [ sel_top ~width:20 t; zero (width - 20) ]
let i_immediate ~width t = sel_top ~width:12 t |> Util.sign_extend ~width
let csr ~width t = uresize ~width (sel_top ~width:12 t)

let s_immediate ~width t =
  concat_msb [ t.:[31, 25]; t.:[11, 7] ] |> Util.sign_extend ~width
;;

let b_immediate ~width t =
  concat_msb [ t.:(31); t.:(7); t.:[30, 25]; t.:[11, 8]; gnd ] |> Util.sign_extend ~width
;;

let j_immediate ~width t =
  concat_msb [ t.:(31); t.:[19, 12]; t.:(20); t.:[30, 21]; gnd ]
  |> Util.sign_extend ~width
;;
