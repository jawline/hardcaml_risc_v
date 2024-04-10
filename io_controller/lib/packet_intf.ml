open Hardcaml_stream

module type S = sig
  module Contents : sig
    type 'a t =
      { data : 'a
      ; last : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  module Contents_stream : Stream_intf.S with type 'a data := 'a Contents.t
end
