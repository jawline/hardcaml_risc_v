open! Core
open Hardcaml

module type Switchable = sig
  type t

  val of_int : int -> t option
  val to_int : t -> int
  val all : t list
end

val switch
  :  (module Switchable with type t = 'a)
  -> f:('a -> Signal.t)
  -> if_not_found:Signal.t
  -> Signal.t
  -> Signal.t

val is : (module Switchable with type t = 'a) -> Signal.t -> 'a -> Signal.t
val sign_extend : width:int -> Signal.t -> Signal.t
