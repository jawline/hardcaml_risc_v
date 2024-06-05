module type S = sig
  type 'a t =
    { (* We could pull old_rd into decoded instruction to avoid the gate on committing *)
      finished : 'a
    ; set_rd : 'a
    ; new_rd : 'a [@bits register_width]
    ; new_pc : 'a [@bits register_width]
    ; error : 'a
    }
  [@@deriving sexp_of, hardcaml]
end
