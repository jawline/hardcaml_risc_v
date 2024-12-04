open! Core
open Hardcaml
open Signal
open Hardcaml_memory_controller
open Hardcaml_framebuffer_expander

module type Config = sig
  val input_width : int
  val input_height : int
  val output_width : int
  val output_height : int
  val framebuffer_address : int
end

module Make (Memory : Memory_bus_intf.S) = struct
  module Screen_signals = struct
    type 'a t =
      { hsync : 'a
      ; vsync : 'a
      ; pixel : 'a
      }
    [@@deriving hardcaml ~rtlmangle:"$"]
  end

  module Video_data = struct
    type 'a t = { vdata : 'a [@bits 24] } [@@deriving hardcaml ~rtlmangle:"$"]
  end

  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; screen : 'a Screen_signals.t
      ; memory_request : 'a Memory.Read_bus.Rx.t
      ; memory_response : 'a Memory.Read_response.With_valid.t
      }
    [@@deriving hardcaml ~rtlmangle:"$"]
  end

  module O = struct
    type 'a t =
      { video_data : 'a Video_data.t
      ; video_signals : 'a Video_signals.Video_signals.t
      ; memory_request : 'a Memory.Read_bus.Tx.t
      }
    [@@deriving hardcaml ~rtlmangle:"$"]
  end

  let create ~framebuffer_config ~video_signals_config scope (i : _ I.t) =
    let module Video_signals_config = (val video_signals_config : Video_signals.Config) in
    let module Video_signals = Video_signals.Make (Video_signals_config) in
    let module Framebuffer_config = (val framebuffer_config : Config) in
    let module Framebuffer_expander =
      Framebuffer_expander.Make (Framebuffer_config) (Memory)
    in
    let video_signals =
      Video_signals.hierarchical
        scope
        { Video_signals.I.clock = i.clock; clear = i.clear }
    in
    let expander =
      Framebuffer_expander.hierarchical
        scope
        { Framebuffer_expander.I.clock = i.clock
        ; clear = i.clear
        ; start = ~:(video_signals.video_active)
        ; next = video_signals.next_pixel
        ; memory_request = i.memory_request
        ; memory_response = i.memory_response
        ; start_address = of_int ~width:32 Framebuffer_config.framebuffer_address
        }
    in
    let pixel_buffer = Fifo.create ~capacity:2048 ~wr:expander.pixel in
    { O.video_data = { vdata = repeat ~count:24 expander.pixel }
    ; video_signals
    ; memory_request = expander.memory_request
    }
  ;;

  let hierarchical ~framebuffer_config ~video_signals_config (scope : Scope.t) =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical
      ~scope
      ~name:"video_out"
      (create ~framebuffer_config ~video_signals_config)
  ;;
end
