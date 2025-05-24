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
      ; argument_1 : 'a [@bits register_width]
        (** [argument_1] is the first argument to the opcode if appropriate. It is decoded from the current registers and the instruction and is opcode specific. *)
      ; argument_2 : 'a [@bits register_width]
        (** [argument_2] is the second argument to the opcode if appropriate. It is decoded from the current registers and the instruction and is opcode specific. *)
      ; argument_3 : 'a [@bits register_width]
        (** [argument_3] is the third argument to the opcode if appropriate. It is decoded from the current registers and the instruction and is opcode specific. *)
      ; rd : 'a [@bits 5]
      ; rd_value : 'a [@bits register_width]
      ; csr : 'a [@bits 12]
      ; is_ecall : 'a
      ; is_csr : 'a
      ; alu_specifics : 'a ALU_specifics.t
      ; error : 'a
      }
    [@@deriving hardcaml]

    val of_instruction : Signal.t -> Signal.t Registers.t -> Scope.t -> Signal.t t
  end
end
