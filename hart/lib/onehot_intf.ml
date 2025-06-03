open! Core
open Hardcaml

module type Config = sig
  type t

  val all : t list

  module Variants : sig
    val to_rank : t -> int
  end
end

module type S = sig
  type base

  val max_rank : int
  val width : int

  type 'a t = { packed : 'a [@bits width] } [@@deriving hardcaml]

  val valid : Signal.t t -> base -> Signal.t
  val construct_onehot : f:(base -> Signal.t) -> Signal.t t
  val construct_onehot_bits : f:(base -> Bits.t) -> Bits.t t
  val switch : f:(base -> Signal.t) -> Signal.t t -> Signal.t
end
