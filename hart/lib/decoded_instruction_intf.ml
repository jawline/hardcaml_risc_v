open Hardcaml

module M (Registers : Registers_intf.S) = struct
  module type S = sig
    module ALU_specifics : sig
      type 'a t =
        { subtract_instead_of_add : 'a
        ; arithmetic_shift : 'a
        }
      [@@deriving hardcaml]
    end

    type 'a t =
      { opcode : 'a Decoded_opcode.Packed.t
      ; funct3 : 'a [@bits 3]
      ; funct7 : 'a [@bits 7]
      ; rs1 : 'a [@bits register_width]
      ; rs2 : 'a [@bits register_width]
      ; rd : 'a [@bits 5]
      ; rd_value : 'a [@bits register_width]
      ; csr : 'a [@bits 12]
      ; i_immediate : 'a [@bits register_width]
      ; j_immediate : 'a [@bits register_width]
      ; s_immediate : 'a [@bits register_width]
      ; u_immediate : 'a [@bits register_width]
      ; b_immediate : 'a [@bits register_width]
      ; load_address : 'a [@bits register_width]
      ; store_address : 'a [@bits register_width]
      ; is_ecall : 'a
      ; is_csr : 'a
      ; alu_specifics : 'a ALU_specifics.t
      ; error : 'a
      }
    [@@deriving hardcaml]

    val of_instruction : Signal.t -> Signal.t Registers.t -> Scope.t -> Signal.t t
  end
end
