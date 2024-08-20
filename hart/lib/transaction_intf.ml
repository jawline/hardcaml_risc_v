open Hardcaml

module type S = sig
  type 'a t =
    { set_rd : 'a
    ; new_rd : 'a [@bits register_width]
    ; new_pc : 'a [@bits register_width]
    ; error : 'a
    }
  [@@deriving sexp_of, hardcaml]

  val generic_error : Signal.t t
end
