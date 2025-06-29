(** Store implements SW, SH and SB. It implements
    a state machine that first collects the current state of the word around
    and address, then writes the new desired word at the align address to a
    register, then waits for the memory controller to write that word out.

    We do not support unaligned writes. *)
open! Core

open Hardcaml
open Hardcaml_memory_controller

module Make (Hart_config : Hart_config_intf.S) (Memory : Memory_bus_intf.S) : sig
  module I : sig
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; enable : 'a
      ; op : 'a Funct3.Store.Onehot.t
      ; destination : 'a [@bits register_width]
      ; value : 'a [@bits register_width]
      ; write_bus : 'a Memory.Write_bus.Dest.t
      ; write_response : 'a Memory.Write_response.With_valid.t
      }
    [@@deriving hardcaml]
  end

  module O : sig
    type 'a t =
      { error : 'a
      ; finished : 'a
      ; write_bus : 'a Memory.Write_bus.Source.t
      }
    [@@deriving hardcaml]
  end

  val hierarchical : Scope.t -> Signal.t I.t -> Signal.t O.t
end
