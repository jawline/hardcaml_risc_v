open! Core
open Hardcaml
open Hardcaml_risc_v
open! Bits

type frame = int array

type t =
  { width : int
  ; height : int
  ; mutable which_cycle : int
  ; mutable which_px : int
  ; mutable current_frame : frame
  ; mutable frames : frame list
  ; mutable was_blanking : bool
  }

let create ~width ~height =
  { width
  ; height
  ; which_cycle = 0
  ; which_px = 0
  ; current_frame = Array.init ~f:(fun _ -> 0) (width * height)
  ; frames = []
  ; was_blanking = false
  }
;;

let commit_frame t = t.frames <- Array.copy t.current_frame :: t.frames

let cycle t ~(video_data : Bits.t) ~(video_signals : Bits.t Video_signals.Video_signals.t)
  =
  if to_bool video_signals.video_active
  then (
    if t.was_blanking
    then (
      commit_frame t;
      t.was_blanking <- false);
    let rgb = to_int_trunc video_data in
    Array.set t.current_frame t.which_px rgb;
    t.which_px <- t.which_px + 1)
  else if to_bool video_signals.v_sync
  then (
    t.was_blanking <- true;
    t.which_px <- 0);
  t.which_cycle <- t.which_cycle + 1
;;
