(** The Load opcode implements LW, LH, LB and non sign extended variants LHU and LBU.
    We currently disallow non-width aligned loads (e.g, shorts at index 1 and words
    at indices 1, 2, 3). *)
open! Core

open Hardcaml
open Hardcaml_memory_controller

module Make (Hart_config : Hart_config_intf.S) (Memory : Memory_bus_intf.S) : sig
  module I : sig
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; enable : 'a
      ; funct3 : 'a
      ; address : 'a
      ; read_bus : 'a Memory.Read_bus.Dest.t
      ; read_response : 'a Memory.Read_response.With_valid.t
      }
    [@@deriving hardcaml]
  end

  module O : sig
    type 'a t =
      { new_rd : 'a
      ; error : 'a
      ; finished : 'a
      ; read_bus : 'a Memory.Read_bus.Source.t
      }
    [@@deriving hardcaml]
  end

  val hierarchical : Scope.t -> Signal.t I.t -> Signal.t O.t
end
