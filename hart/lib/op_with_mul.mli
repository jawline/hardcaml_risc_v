(** Wraps op in a multiplier (in Risc-V the muldiv and standard op instructions
 * are controlled by funct7). *)
open! Core

open Hardcaml

module Make (Hart_config : Hart_config_intf.S) : sig
  module I : sig
    type 'a t =
      { alu_op : 'a Alu_operation.Onehot.t
      ; muldiv_op : 'a Funct3.Muldiv.Onehot.t
      ; is_muldiv : 'a
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
