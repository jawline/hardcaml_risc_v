open! Core
open Hardcaml
open Signal
open Hardcaml_memory_controller
open Hardcaml_framebuffer_expander

module Make
    (Config : sig
       val input_width : int
       val input_height : int
       val output_width : int
       val output_height : int
       val framebuffer_address : int
     end)
    (Memory : Memory_bus_intf.S) =
struct
  module Framebuffer_expander = Framebuffer_expander.Make (Config) (Memory)

  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; hsync : 'a
      ; vsync : 'a
      ; pixel : 'a
      ; memory_request : 'a Memory.Read_bus.Rx.t
      ; memory_response : 'a Memory.Read_response.With_valid.t
      }
    [@@deriving hardcaml ~rtlmangle:"$"]
  end

  module O = struct
    type 'a t =
      { vdata : 'a [@bits 24]
      ; memory_request : 'a Memory.Read_bus.Tx.t
      }
    [@@deriving hardcaml ~rtlmangle:"$"]
  end

  let create scope (i : _ I.t) =
    let expander =
      Framebuffer_expander.hierarchical
        scope
        { Framebuffer_expander.I.clock = i.clock
        ; clear = i.clear
        ; start = i.vsync
        ; next = i.pixel
        ; memory_request = i.memory_request
        ; memory_response = i.memory_response
        ; start_address = of_int ~width:32 Config.framebuffer_address
        }
    in
    { O.vdata = List.init ~f:(fun _ -> expander.pixel) 24 |> concat_lsb
    ; memory_request = expander.memory_request
    }
  ;;

  let hierarchical (scope : Scope.t) (input : Signal.t I.t) =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ~scope ~name:"video_out" create input
  ;;
end
