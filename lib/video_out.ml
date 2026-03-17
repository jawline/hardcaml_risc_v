open! Core
open Hardcaml
open Signal
open Hardcaml_memory_controller
open Hardcaml_framebuffer_expander

module Make (Memory : Memory_bus_intf.S) = struct
  module Video_data = struct
    type 'a t = { vdata : 'a [@bits 24] } [@@deriving hardcaml ~rtlmangle:"$"]
  end

  module I = struct
    type 'a t =
      { clock : 'a Clocking.t
      ; memory_request : 'a Memory.Read_bus.Dest.t
      ; memory_response : 'a Memory.Read_response.With_valid.t
      }
    [@@deriving hardcaml ~rtlmangle:"$"]
  end

  module O = struct
    type 'a t =
      { video_data : 'a Video_data.t
      ; video_signals : 'a Video_signals.Video_signals.t
      ; memory_request : 'a Memory.Read_bus.Source.t
      }
    [@@deriving hardcaml ~rtlmangle:"$"]
  end

  let create ~framebuffer_config ~video_signals_config scope (i : _ I.t) =
    let module Video_signals_config = (val video_signals_config : Video_signals.Config) in
    let module Video_signals = Video_signals.Make (Video_signals_config) in
    let module Framebuffer_config = (val framebuffer_config : Video_out_intf.Config) in
    let module Framebuffer_expander =
      Framebuffer_expander.Make (Framebuffer_config) (Memory)
    in
    let video_signals =
      Video_signals.hierarchical scope { Video_signals.I.clock = i.clock }
    in
    let pixel_buffer_full = wire 1 in
    let expander_valid = wire 1 in
    let pre_fetch_pixel = expander_valid &: ~:pixel_buffer_full in
    let expander =
      Framebuffer_expander.hierarchical
        scope
        { Framebuffer_expander.I.clock = i.clock
        ; start = video_signals.next_frame
        ; next = pre_fetch_pixel
        ; memory_request = i.memory_request
        ; memory_response = i.memory_response
        ; start_address = of_unsigned_int ~width:32 Framebuffer_config.framebuffer_address
        }
    in
    expander_valid <-- expander.valid;
    (* TODO: Rather than pre-fetching here it would be a lot more efficient in
       memory to pre-fetch words in the framebuffer expander. *)
    let pixel_buffer =
      Fifo.create
        ~showahead:true
        ~clock:i.clock.clock
        ~clear:(i.clock.clear |: video_signals.next_frame)
        ~capacity:4096
        ~wr:pre_fetch_pixel
        ~d:(concat_lsb [ expander.pixel.r; expander.pixel.g; expander.pixel.b ])
        ~rd:(video_signals.video_clock &: video_signals.video_active)
        ()
    in
    pixel_buffer_full <-- pixel_buffer.full;
    { O.video_data = { vdata = pixel_buffer.q }
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
