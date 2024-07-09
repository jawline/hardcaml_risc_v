open! Core
open Hardcaml
open Signal

type 'a t =
  { is_ecall : 'a
  ; is_store : 'a
  ; is_load : 'a
  }
[@@deriving sexp_of, hardcaml ~rtlmangle:"$"]

let of_instruction instruction _registers _scope =
  let is_ecall =
    let system = Decoder.opcode instruction ==:. Opcodes.system in
    let ecall =
      Decoder.funct3 instruction ==:. Funct3.System.to_int Funct3.System.Ecall_or_ebreak
    in
    system &: ecall
  in
  { is_ecall
  ; is_store = Decoder.opcode instruction ==:. Opcodes.store
  ; is_load = Decoder.opcode instruction ==:. Opcodes.load
  }
;;
