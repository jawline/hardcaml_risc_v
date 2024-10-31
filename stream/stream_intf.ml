module type S = sig
  type 'a data

  module Tx : sig
    type 'a t =
      { valid : 'a
      ; data : 'a data
      }
    [@@deriving hardcaml]
  end

  module Rx : sig
    type 'a t = { ready : 'a } [@@deriving hardcaml]
  end
end
