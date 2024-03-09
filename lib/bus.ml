open Hardcaml

module Make (M : sig
  module Data : Interface.S
end) =
struct
  module Tx = struct
    type 'a t =
      { valid : 'a
      ; data : 'a M.Data.t
      }
    [@@deriving sexp_of, hardcaml]
  end

  module Rx = struct
    type 'a t = { ready : 'a } [@@deriving sexp_of, hardcaml]
  end
end
