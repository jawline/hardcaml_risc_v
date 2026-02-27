(** A framebuffer expander that reads a greyscale bitvector of input_width *
    input_height from the memory controller and outputs it pixel by pixel width
    by height.

    At the start of the frame start should be called, providing some cycles of
    leeway before the first pixel is necessary for prefetching.

    To move on to the next pixel the next input should be set high for a cycle.
    As with start, the next pixel should be sampled some cycles after the next
    pulse to provide leeway for fetching.

    Row memory must be word aligned (e.g, if a row is 8 width it must still be
    4 bytes long). *)
open! Core

open Hardcaml
open Hardcaml_memory_controller

module Make
    (_ : sig
       val input_width : int
       val input_height : int
       val output_width : int
       val output_height : int
       val input_pixel_mode : Pixel_mode.t
     end)
    (Memory : Memory_bus_intf.S) : sig
  module Pixel : sig
    type 'a t =
      { r : 'a [@bits 8]
      ; g : 'a [@bits 8]
      ; b : 'a [@bits 8]
      }
    [@@deriving hardcaml]
  end

  module I : sig
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; start : 'a
      ; next : 'a
      ; start_address : 'a
      ; memory_request : 'a Memory.Read_bus.Dest.t
      ; memory_response : 'a Memory.Read_response.With_valid.t
      }
    [@@deriving hardcaml]
  end

  module O : sig
    type 'a t =
      { valid : 'a
      ; pixel : 'a Pixel.t
      ; memory_request : 'a Memory.Read_bus.Source.t
      }
    [@@deriving hardcaml]
  end

  val scaling_factor_x : int
  val scaling_factor_y : int
  val margin_x_start : int
  val margin_y_start : int
  val margin_x_end : int
  val margin_y_end : int
  val hierarchical : Scope.t -> Signal.t I.t -> Signal.t O.t
end
