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
      ; memory_request : 'a Memory.Read_bus.Tx.t
      }
    [@@deriving hardcaml ~rtlmangle:"$"]
  end

  let create ~config scope (i : _ I.t) =
    let module Config = (val config : Config) in
    let module Framebuffer_expander = Framebuffer_expander.Make (Config) (Memory) in
    let expander =
      Framebuffer_expander.hierarchical
        scope
        { Framebuffer_expander.I.clock = i.clock
        ; clear = i.clear
        ; start = i.screen.vsync
        ; next = i.screen.pixel
        ; memory_request = i.memory_request
        ; memory_response = i.memory_response
        ; start_address = of_int ~width:32 Config.framebuffer_address
        }
    in
    { O.video_data = { vdata = List.init ~f:(fun _ -> expander.pixel) 24 |> concat_lsb }
    ; memory_request = expander.memory_request
    }
  ;;

  let hierarchical ~config (scope : Scope.t) =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ~scope ~name:"video_out" (create ~config)
  ;;
end
