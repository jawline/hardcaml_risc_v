open Hardcaml
module Make (M : Interface.S) : Handshake_intf.S with type 'a data = 'a M.t
