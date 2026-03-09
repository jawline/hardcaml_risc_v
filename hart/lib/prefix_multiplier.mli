(* This is a slow (four cycle) partial product multiplier. We split the inputs
   into partial products because even with cascading the DSP units on Xilinx
   FPGA do not meet reasonable timing on the 7 line. 
     
   This multiplier takes an LHS and an RHS and uses the partial product
   accumulation over four cycles:
   lo, hi = split lhs, rhs
   s0 = lo * lo
   s1 = hi1 * lo2 << width / 2
   s2 = hi2 * lo1 << width / 2
   s3 = hi * hi
   result = sum (s0, s1, s2, s3)

*)
open! Core
open Hardcaml

module Make (C : sig
    val width : int
  end) : sig
  module I : sig
    type 'a t =
      { clock : 'a Clocking.t
      ; valid : 'a
      ; lhs : 'a
      ; rhs : 'a
      }
    [@@deriving hardcaml]
  end

  module O : sig
    type 'a t =
      { valid : 'a
      ; value : 'a
      }
    [@@deriving hardcaml]
  end

  val hierarchical : Scope.t -> Signal.t I.t -> Signal.t O.t
end
