open Hardcaml

module Make (M : sig
    module Data : sig
      type 'a t [@@deriving sexp]

      include Interface.S with type 'a t := 'a t
    end
  end) =
struct
  module Tx = struct
    type 'a t =
      { valid : 'a
      ; data : 'a M.Data.t
      }
    [@@deriving hardcaml]
  end

  module Rx = struct
    type 'a t = { ready : 'a } [@@deriving hardcaml]
  end
end
