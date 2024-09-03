(** This is an IO module that pulses a signal once per packet it receives. *)
open! Core

open Hardcaml
open Signal

module Make (P : Packet_intf.S) = struct
  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; in_ : 'a P.Contents_stream.Tx.t
      }
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t =
      { in_ : 'a P.Contents_stream.Rx.t
      ; signal : 'a
      }
    [@@deriving hardcaml]
  end

  let create (_scope : Scope.t) ({ I.clock = _; clear = _; in_ } : _ I.t) =
    { O.in_ = { ready = vdd }; signal = in_.valid &: in_.data.last }
  ;;

  let hierarchical ~instance (scope : Scope.t) (input : Signal.t I.t) =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ~scope ~name:"pulse" ~instance create input
  ;;
end
