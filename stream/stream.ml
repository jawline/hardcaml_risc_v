open Hardcaml

module Make (M : Interface.S) = struct
  type 'a data = 'a M.t

  module Tx = struct
    type 'a t =
      { valid : 'a
      ; data : 'a M.t
      }
    [@@deriving hardcaml]
  end

  module Rx = struct
    type 'a t = { ready : 'a } [@@deriving hardcaml]
  end
end
