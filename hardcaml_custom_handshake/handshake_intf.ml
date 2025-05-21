module type S = sig
  type 'a data

  module Source : sig
    type 'a t =
      { valid : 'a
      ; data : 'a data
      }
    [@@deriving hardcaml]
  end

  module Dest : sig
    type 'a t = { ready : 'a } [@@deriving hardcaml]
  end
end
