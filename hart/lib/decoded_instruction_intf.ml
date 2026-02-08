open Hardcaml

module M (Registers : Registers_intf.S) = struct
  module type S = sig
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
      ; alu_operation : 'a Alu_operation.Onehot.t
        (** The ALU operation onehot encodes what the ALU should do if Decoded_opcode is ALU. The includes the pre-coded Add | Sub and Srl | Sra operations. *)
      ; muldiv_operation : 'a Funct3.Muldiv.Onehot.t option
      ; is_muldiv : 'a option
        (** Set if the ALU instruction is Muldiv (opcode was Op and funct7 is 1). *)
      ; branch_onehot : 'a Funct3.Branch.Onehot.t
      ; load_onehot : 'a Funct3.Load.Onehot.t
      ; store_onehot : 'a Funct3.Store.Onehot.t
      ; error : 'a
      }
    [@@deriving hardcaml]

    module With_valid : With_valid.Wrap.S with type 'a value = 'a t

    val of_instruction : Signal.t -> Signal.t Registers.t -> Scope.t -> Signal.t t
  end
end
