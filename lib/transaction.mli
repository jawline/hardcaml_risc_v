open! Core
open! Hardcaml

module Make (Hart_config : Hart_config_intf.S) : sig
  type 'a t =
    { set_rd : 'a
    ; new_rd : 'a [@bits register_width]
    ; new_pc : 'a [@bits register_width]
    ; error : 'a
    }
  [@@deriving sexp_of, hardcaml]
end
