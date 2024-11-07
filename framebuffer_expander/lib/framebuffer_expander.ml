open Core
open Hardcaml
open Signal
open Hardcaml_memory_controller

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

  module State = struct
    type t =
      | Y_margin_start
      | Y_margin_end
      | X_margin_start
      | X_body
      | X_margin_end
    [@@deriving compare, enumerate, sexp_of]
  end

  let create _scope (i : _ I.t) =
    let open Always in
    let reg_spec = Reg_spec.create ~clock:i.clock ~clear:i.clear () in
    let reg_x = Variable.reg ~width:(num_bits_to_represent input_width) reg_spec in
    let reg_y = Variable.reg ~width:(num_bits_to_represent input_height) reg_spec in
    let x_ctr =
      Variable.reg
        ~width:(num_bits_to_represent (Int.max margin_x_start margin_x_end - 1))
        reg_spec
    in
    let y_line_ctr =
      Variable.reg
        ~width:(num_bits_to_represent (Int.max margin_y_start margin_y_end - 1))
        reg_spec
    in
    let y_px_ctr =
      Variable.reg ~width:(num_bits_to_represent (output_width - 1)) reg_spec
    in
    let current_state = State_machine.create (module State) reg_spec in
    let start =
      let enter_state =
        if margin_y_start = 0
        then proc [ current_state.set_next X_margin_start ]
        else proc [ current_state.set_next Y_margin_start ]
      in
      proc
        [ reg_x <--. 0
        ; reg_y <--. 0
        ; x_ctr <--. 0
        ; y_line_ctr <--. 0
        ; y_px_ctr <--. 0
        ; enter_state
        ]
    in
    let y_margin_line ~stop_condition ~stop_behaviour =
      proc
        [ incr y_px_ctr
        ; when_
            (y_px_ctr.value ==:. output_width - 1)
            [ incr y_line_ctr
            ; y_px_ctr <--. 0
            ; when_
                (y_line_ctr.value ==:. stop_condition - 1)
                [ y_line_ctr <--. 0; stop_behaviour ]
            ]
        ]
    in
    let x_margin ~size ~stop_behaviour =
      proc
        [ incr x_ctr; when_ (x_ctr.value ==:. size - 1) [ x_ctr <--. 0; stop_behaviour ] ]
    in
    let enter_x_line =
      if margin_x_start = 0
      then current_state.set_next X_body
      else current_state.set_next X_margin_start
    in
    let proceed =
      current_state.switch
        [ ( State.Y_margin_start
          , [ y_margin_line ~stop_condition:margin_y_start ~stop_behaviour:enter_x_line ]
          )
        ; ( State.X_margin_start
          , [ x_margin
                ~size:margin_x_start
                ~stop_behaviour:(proc [ current_state.set_next X_body ])
            ] )
        ; State.X_body, assert false
        ; ( State.X_margin_end
          , [ x_margin
                ~size:margin_x_end
                ~stop_behaviour:
                  (proc
                     [ assert false
                       (* If move on to next line then (if next line is end
                          then y margin end else y += 1 *)
                     ; enter_x_line
                     ])
            ] )
        ; ( State.Y_margin_end
          , [ y_margin_line ~stop_condition:margin_y_end ~stop_behaviour:(proc []) ] )
        ]
    in
    compile [ when_ i.next [ proceed ]; when_ i.start [ start ] ];
    { O.pixel = mux2 (current_state.is X_body) (assert false) gnd
    ; memory_request = assert false
    }
  ;;
end

let%expect_test "margins and scaling factor tests" =
  let module Memory_controller =
    Memory_controller.Make (struct
      let capacity_in_bytes = 256
      let num_write_channels = 1
      let num_read_channels = 1
      let address_width = 32
      let data_bus_width = 32
    end)
  in
  let module M =
    Make
      (struct
        let input_width = 64
        let input_height = 32
        let output_width = 133
        let output_height = 35
      end)
      (Memory_controller.Memory_bus)
  in
  print_s
    [%message
      (M.scaling_factor_x : int)
        (M.scaling_factor_y : int)
        (M.margin_x_start : int)
        (M.margin_x_end : int)
        (M.margin_y_start : int)
        (M.margin_y_end : int)];
  [%expect
    {|
    ((M.scaling_factor_x 1) (M.scaling_factor_y 1) (M.margin_x_start 2)
     (M.margin_x_end 1) (M.margin_y_start 2) (M.margin_y_end 1))
    |}]
;;

let%expect_test "margins and scaling factor tests" =
  let module Memory_controller =
    Memory_controller.Make (struct
      let capacity_in_bytes = 256
      let num_write_channels = 1
      let num_read_channels = 1
      let address_width = 32
      let data_bus_width = 32
    end)
  in
  let module M =
    Make
      (struct
        let input_width = 64
        let input_height = 32
        let output_width = 1280
        let output_height = 720
      end)
      (Memory_controller.Memory_bus)
  in
  print_s
    [%message
      (M.scaling_factor_x : int)
        (M.scaling_factor_y : int)
        (M.margin_x_start : int)
        (M.margin_x_end : int)
        (M.margin_y_start : int)
        (M.margin_y_end : int)];
  [%expect
    {|
    ((M.scaling_factor_x 1) (M.scaling_factor_y 1) (M.margin_x_start 2)
     (M.margin_x_end 1) (M.margin_y_start 2) (M.margin_y_end 1))
    |}]
;;
