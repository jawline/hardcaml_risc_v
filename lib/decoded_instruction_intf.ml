module type S = sig
  type 'a t =
    { opcode : 'a
    ; funct3 : 'a
    ; funct7 : 'a
    ; rs1 : 'a
    ; rs2 : 'a
    ; rd : 'a
    }
  [@@deriving sexp_of, hardcaml]
end
