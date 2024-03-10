module type S = sig
  type 'a data

  module Tx : sig
    type 'a t =
      { valid : 'a
      ; data : 'a data
      }
    [@@deriving sexp_of, hardcaml]
  end

  module Rx : sig
    type 'a t = { ready : 'a } [@@deriving sexp_of, hardcaml]
  end
end
