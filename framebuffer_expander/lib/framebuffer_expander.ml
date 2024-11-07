open Core
open Hardcaml
open Signal

(** A framebuffer expander that reads a greyscale bitvector of input_width *
    input_height from the memory controller and outputs it pixel by pixel width
    by height.

    At the start of the frame start should be called, providing some cycles of
    leeway before the first pixel is necessary for prefetching.

    To move on to the next pixel the next input should be set high for a cycle.
    As with start, the next pixel should be sampled some cycles after the next
    pulse to provide leeway for fetching. *)
module Make
    (Config : sig
       val input_width : int
       val input_height : int
       val output_width : int
       val output_height : int
     end)
    (Memory : Memory_bus_intf.S) =
struct
  open Config

  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; start : 'a
      ; next : 'a
      ; start_address : 'a [@bits Memory.Read_bus.Tx.port_widths.address]
      ; memory_request : 'a Memory.Read_bus.Rx.t
      ; memory_response : 'a Memory.Read_response.With_valid.t
      }
  end

  module O = struct
    type 'a t =
      { pixel : 'a
      ; memory_request : 'a Memory.Read_bus.Tx.t
      }
  end

  let () =
    if input_width > output_width || input_height > output_height
    then raise_s [%message "inputs must be smaller than outputs"]
  ;;

  let scaling_factor_x, scaling_factor_y =
    output_width / input_width, output_height / input_height
  ;;

  let margin_x_start, margin_x_end =
    let extra_x = output_width % input_width in
    let margin_x_end = extra_x / 2 in
    let margin_x_start = margin_x_end + Bool.to_int (extra_x % 2 <> 0) in
    margin_x_start, margin_x_end
  ;;

  let margin_y_start, margin_y_end =
    let extra_y = output_height % input_height in
    let margin_y_end = extra_y / 2 in
    let margin_y_start = margin_y_end + Bool.to_int (extra_y % 2 <> 0) in
    margin_y_start, margin_y_end
  ;;

  let create scope (i : _ I.t) = assert false
end
