open! Core
open Hardcaml
open Hardcaml_memory_controller

module type Config = sig
  val input_width : int
  val input_height : int
  val output_width : int
  val output_height : int
  val framebuffer_address : int
end

module M (Memory : Memory_bus_intf.S) = struct
  module type S = sig
    module Video_data : sig
      type 'a t = { vdata : 'a } [@@deriving hardcaml]
    end

    module I : sig
      type 'a t =
        { clock : 'a
        ; clear : 'a
        ; memory_request : 'a Memory.Read_bus.Dest.t
        ; memory_response : 'a Memory.Read_response.With_valid.t
        }
      [@@deriving hardcaml]
    end

    module O : sig
      type 'a t =
        { video_data : 'a Video_data.t
        ; video_signals : 'a Video_signals.Video_signals.t
        ; memory_request : 'a Memory.Read_bus.Source.t
        }
      [@@deriving hardcaml]
    end

    val hierarchical
      :  framebuffer_config:(module Config)
      -> video_signals_config:(module Video_signals.Config)
      -> Scope.t
      -> Signal.t I.t
      -> Signal.t O.t
  end
end
