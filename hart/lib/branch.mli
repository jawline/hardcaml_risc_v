open! Core
open Hardcaml

module Make (Hart_config : Hart_config_intf.S) : sig
  module I : sig
    type 'a t =
      { pc : 'a [@bits register_width]
      ; lhs : 'a [@bits register_width]
      ; rhs : 'a [@bits register_width]
      ; funct3 : 'a Funct3.Branch.Onehot.t
      ; branch_offset : 'a [@bits register_width]
      }
    [@@deriving hardcaml]
  end

  module O : sig
    type 'a t = { new_pc : 'a [@bits register_width] } [@@deriving hardcaml]
  end

  val hierarchical : instance:string -> Scope.t -> Signal.t I.t -> Signal.t O.t
end
