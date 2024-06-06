open! Core
open Hardcaml

module M
    (Registers : Registers_intf.S)
    (Decoded_instruction : Decoded_instruction_intf.M(Registers).S)
    (Transaction : Transaction_intf.S) =
struct
  module type S = sig
    val handler
      :  registers:Signal.t Registers.t
      -> decoded_instruction:Signal.t Decoded_instruction.t
      -> Signal.t Transaction.t
  end
end
