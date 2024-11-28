open Hardcaml

module type S = sig
  type 'a t =
    { set_rd : 'a
    ; new_rd : 'a [@bits register_width]
    ; new_pc : 'a [@bits register_width]
    ; error : 'a
    }
  [@@deriving hardcaml]

  val generic_error : Signal.t t

  module With_valid : With_valid.Wrap.S with type 'a value := 'a t
end
