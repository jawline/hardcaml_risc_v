open! Core
open Hardcaml

module Make (Hart_config : Hart_config_intf.S) : sig
  module I : sig
    type 'a t =
      { op : 'a Alu_operation.Onehot.t
      ; lhs : 'a
      ; rhs : 'a
      }
    [@@deriving hardcaml]
  end

  module O : sig
    type 'a t = { rd : 'a [@bits register_width] } [@@deriving hardcaml]
  end

  val hierarchical : Scope.t -> Signal.t I.t -> Signal.t O.t
end
