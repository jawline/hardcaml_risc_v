open Hardcaml

module Make (M : sig
  module Data : Interface.S
end) =
struct
  type data = M.Data.t

  module Tx = struct
    type 'a t =
      { valid : 'a
      ; data : 'a data
      }
    [@@deriving sexp_of, hardcaml]
  end

  module Rx = struct
    type 'a t = { ready : 'a } [@@deriving sexp_of, hardcaml]
  end
end
