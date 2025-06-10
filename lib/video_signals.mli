(* This module generates h_sync, v_sync, v_pixel, and next_frame signals from the provided  assuming that the clock input is the same as the pixel clock. *)
open! Core
open Hardcaml

module type Config = sig
  val h_active : int
  val h_fp : int
  val h_sync : int
  val h_bp : int
  val v_active : int
  val v_fp : int
  val v_sync : int
  val v_bp : int
end

module Video_signals : sig
  type 'a t =
    { video_active : 'a
    ; v_sync : 'a
    ; h_sync : 'a
    ; next_frame : 'a
    }
  [@@deriving hardcaml]
end

module Make (Config : Config) : sig
  module I : sig
    type 'a t =
      { clock : 'a
      ; clear : 'a
      }
    [@@deriving hardcaml]
  end

  module O = Video_signals

  val hierarchical : Scope.t -> Signal.t I.t -> Signal.t O.t
end
