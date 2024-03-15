open! Core
open! Hardcaml

module Make (Hart_config : Hart_config_intf.S) = struct
  let register_width = Register_width.bits Hart_config.register_width

  type 'a t =
    { (* We could pull old_rd into decoded instruction to avoid the gate on committing *)
      set_rd : 'a
    ; new_rd : 'a [@bits register_width]
    ; new_pc : 'a [@bits register_width]
    ; error : 'a
    }
  [@@deriving sexp_of, hardcaml]
end
