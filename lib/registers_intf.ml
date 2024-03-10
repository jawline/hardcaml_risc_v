module type S = sig
  type 'a t =
    { pc : 'a
    ; general : 'a list
    }
  [@@deriving sexp_of, hardcaml]
end
