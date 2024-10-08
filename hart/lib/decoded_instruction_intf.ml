open Hardcaml

module M (Registers : Registers_intf.S) = struct
  module type S = sig
    type 'a t =
      { opcode : 'a [@bits 7]
      ; funct3 : 'a [@bits 3]
      ; funct7 : 'a [@bits 7]
      ; rs1 : 'a [@bits register_width]
      ; rs2 : 'a [@bits register_width]
      ; rd : 'a [@bits 5]
      ; rd_value : 'a [@bits register_width]
      ; i_immediate : 'a [@bits register_width]
      ; j_immediate : 'a [@bits register_width]
      ; s_immediate : 'a [@bits register_width]
      ; u_immediate : 'a [@bits register_width]
      ; b_immediate : 'a [@bits register_width]
      ; load_address : 'a [@bits register_width]
      ; store_address : 'a [@bits register_width]
      ; funct7_switch : 'a
      ; funct7_bit_other_than_switch_is_selected : 'a
      ; is_ecall : 'a
      ; decoded_opcode_or_error : 'a [@bits Opcodes.Or_error.bits_to_repr]
      ; opcode_signals : 'a Opcodes.Signals.t
      }
    [@@deriving hardcaml]

    val of_instruction : Signal.t -> Signal.t Registers.t -> Scope.t -> Signal.t t
  end
end
