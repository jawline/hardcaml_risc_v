module type S = sig
  type 'a t =
    { opcode : 'a
    ; funct3 : 'a
    ; funct7 : 'a
    ; rs1 : 'a
    ; rs2 : 'a
    ; rd : 'a
    ; i_immediate : 'a
    ; j_immediate : 'a
    ; u_immediate : 'a
    ; b_immediate : 'a
    }
  [@@deriving sexp_of, hardcaml]
end
