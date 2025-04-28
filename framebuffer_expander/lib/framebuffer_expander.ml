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

open Core
open Hardcaml
open Signal
open Hardcaml_memory_controller

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
      ; start_address : 'a [@bits Memory.Read_bus.Tx.port_widths.data.address]
      ; memory_request : 'a Memory.Read_bus.Rx.t
      ; memory_response : 'a Memory.Read_response.With_valid.t
      }
    [@@deriving hardcaml ~rtlmangle:"$"]
  end

  module O = struct
    type 'a t =
      { valid : 'a
      ; pixel : 'a
      ; memory_request : 'a Memory.Read_bus.Tx.t
      }
    [@@deriving hardcaml ~rtlmangle:"$"]
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

  (* We require that row memory be word aligned (e.g, even if row width % 4 <>
     0 we still say the row is a multiple of 4 bytes wide).

     This greatly simplifies thinking about framebuffer row offsets since
     bitvectors are always word aligned. *)

  let word_in_bytes = I.port_widths.start_address / 8

  let row_offset_in_words =
    (* Number of bytes in a row, aligned to the nearest byte (8 bits). *)
    let row_num_bytes =
      let whole_component = input_width / 8 in
      let ratio_component = input_height % 8 in
      whole_component + if ratio_component <> 0 then 1 else 0
    in
    let whole_component = row_num_bytes / word_in_bytes in
    let ratio_component = row_num_bytes % word_in_bytes in
    whole_component + if ratio_component <> 0 then 1 else 0
  ;;

  let row_offset_in_bytes = row_offset_in_words * word_in_bytes

  let create scope (i : _ I.t) =
    let open Always in
    let reg_spec = Reg_spec.create ~clock:i.clock ~clear:i.clear () in
    let reg_spec_no_clear = Reg_spec.create ~clock:i.clock () in
    (* Reg_x and reg_y track where in the input framebuffer we are *)
    let%hw_var reg_x =
      Variable.reg ~width:(num_bits_to_represent input_width) reg_spec_no_clear
    in
    let%hw_var reg_y =
      Variable.reg ~width:(num_bits_to_represent input_height) reg_spec_no_clear
    in
    (* Next_address is the address into the current line, row_start_address is
       the address to return to if we are repeating a line *)
    let%hw_var next_address =
      Variable.reg ~width:I.port_widths.start_address reg_spec_no_clear
    in
    let%hw_var row_start_address =
      Variable.reg ~width:I.port_widths.start_address reg_spec_no_clear
    in
    (* This counters are used to keep track of where we are when doubling a
       pixel or adding a margin line. *)
    let%hw_var y_px_ctr =
      let max_elt = Int.max (output_width - 1) (scaling_factor_y - 1) in
      Variable.reg ~width:(num_bits_to_represent max_elt) reg_spec_no_clear
    in
    let%hw_var x_px_ctr =
      let max_elt =
        let margins = Int.max margin_x_start margin_x_end - 1 in
        let scaling = scaling_factor_x - 1 in
        Int.max margins scaling
      in
      Variable.reg ~width:(num_bits_to_represent max_elt) reg_spec_no_clear
    in
    let%hw_var y_line_ctr =
      let max_elt =
        let margins = Int.max margin_y_start margin_y_end - 1 in
        let min = 1 in
        Int.max margins min
      in
      Variable.reg ~width:(num_bits_to_represent max_elt) reg_spec_no_clear
    in
    (* When not fetched we will prefetch the next data byte of the body. *)
    let%hw_var fetched = Variable.reg ~width:1 reg_spec_no_clear in
    let%hw_var data_valid = Variable.reg ~width:1 reg_spec_no_clear in
    let%hw_var data =
      Variable.reg ~width:I.port_widths.memory_response.value.read_data reg_spec_no_clear
    in
    let current_state = State_machine.create (module State) reg_spec in
    ignore (current_state.current -- "current_state" : Signal.t);
    let%hw which_bit =
      let num_bits = num_bits_to_represent (width data.value - 1) in
      if num_bits > width data.value
      then sel_bottom ~width:num_bits reg_x.value
      else uresize ~width:num_bits reg_x.value
    in
    let enter_x_line =
      if margin_x_start = 0
      then current_state.set_next X_body
      else current_state.set_next X_margin_start
    in
    let clear_fetched = proc [ data_valid <-- gnd; fetched <-- gnd ] in
    let start =
      let enter_state =
        (if margin_y_start = 0
         then [ enter_x_line ]
         else [ current_state.set_next Y_margin_start ])
        |> proc
      in
      proc
        [ reg_x <--. 0
        ; reg_y <--. 0
        ; x_px_ctr <--. 0
        ; y_line_ctr <--. 0
        ; y_px_ctr <--. 0
        ; next_address <-- i.start_address
        ; row_start_address <-- i.start_address
        ; clear_fetched
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
        [ incr x_px_ctr
        ; when_ (x_px_ctr.value ==:. size - 1) [ x_px_ctr <--. 0; stop_behaviour ]
        ]
    in
    let move_on_to_next_y_row =
      let next_row_address =
        reg reg_spec_no_clear (row_start_address.value +:. row_offset_in_bytes)
      in
      let at_y_row_limit = reg_y.value ==:. input_height - 1 in
      [ y_px_ctr <--. 0
      ; incr reg_y
      ; next_address <-- next_row_address
      ; row_start_address <-- next_row_address
      ; when_
          at_y_row_limit
          [ reg_y <--. 0
          ; (* Even though Y_margin_end might be a no-op it is always ok to go
               on to it since a start signal will then restart for the next frame.
            *)
            current_state.set_next Y_margin_end
          ]
      ]
      |> proc
    in
    let move_on_or_repeat_y_row =
      proc
        [ enter_x_line
        ; incr y_px_ctr
        ; next_address <-- row_start_address.value
        ; clear_fetched
        ; when_ (y_px_ctr.value ==:. scaling_factor_y - 1) [ move_on_to_next_y_row ]
        ]
    in
    let end_x_line =
      if margin_x_end = 0
      then move_on_or_repeat_y_row
      else current_state.set_next X_margin_end
    in
    let x_body_next_pixel =
      let move_on_to_next_part = end_x_line in
      proc
        [ incr x_px_ctr
        ; when_
            (x_px_ctr.value ==:. scaling_factor_x - 1)
            [ x_px_ctr <--. 0
            ; incr reg_x
            ; when_
                (which_bit ==: ones (width which_bit))
                [ next_address <-- reg reg_spec_no_clear (next_address.value +:. 4)
                ; clear_fetched
                ]
            ; when_
                (reg_x.value ==:. input_width - 1)
                [ reg_x <--. 0; move_on_to_next_part ]
            ]
        ]
    in
    let proceed =
      current_state.switch
        [ ( State.Y_margin_start
          , if margin_y_start = 0
            then [ enter_x_line ]
            else
              [ y_margin_line ~stop_condition:margin_y_start ~stop_behaviour:enter_x_line
              ] )
        ; ( State.X_margin_start
          , if margin_x_start = 0
            then []
            else
              [ x_margin
                  ~size:margin_x_start
                  ~stop_behaviour:(proc [ current_state.set_next X_body ])
              ] )
        ; State.X_body, [ x_body_next_pixel ]
        ; ( State.X_margin_end
          , if margin_x_end = 0
            then []
            else [ x_margin ~size:margin_x_end ~stop_behaviour:move_on_or_repeat_y_row ]
          )
        ; ( State.Y_margin_end
          , if margin_y_end <> 0
            then [ y_margin_line ~stop_condition:margin_y_end ~stop_behaviour:(proc []) ]
            else [] )
        ]
    in
    let request_read = current_state.is X_body &: ~:(fetched.value) in
    compile
      [ when_ i.next [ proceed ]
      ; when_ i.start [ start ]
      ; when_ (request_read &: i.memory_request.ready) [ fetched <-- vdd ]
      ; when_
          i.memory_response.valid
          [ data_valid <-- vdd; data <-- i.memory_response.value.read_data ]
      ];
    let body_bit =
      mux_init ~f:(fun i -> bit ~pos:i data.value) which_bit (width data.value)
    in
    { O.valid =
        ~:(current_state.is X_body) |: (current_state.is X_body &: data_valid.value)
    ; pixel = mux2 (current_state.is X_body) body_bit gnd
    ; memory_request =
        { Memory.Read_bus.Tx.valid = request_read
        ; data = { address = next_address.value }
        }
    }
  ;;

  let hierarchical (scope : Scope.t) (input : Signal.t I.t) =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ~scope ~name:"framebuffer_expander" create input
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
        let input_width = 3
        let input_height = 3
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
        (M.margin_y_end : int)
        (M.word_in_bytes : int)
        (M.row_offset_in_bytes : int)
        (M.row_offset_in_words : int)];
  [%expect
    {|
    ((M.scaling_factor_x 44) (M.scaling_factor_y 11) (M.margin_x_start 1)
     (M.margin_x_end 0) (M.margin_y_start 1) (M.margin_y_end 1)
     (M.word_in_bytes 4) (M.row_offset_in_bytes 4) (M.row_offset_in_words 1))
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
        (M.margin_y_end : int)
        (M.word_in_bytes : int)
        (M.row_offset_in_bytes : int)
        (M.row_offset_in_words : int)];
  [%expect
    {|
    ((M.scaling_factor_x 20) (M.scaling_factor_y 22) (M.margin_x_start 0)
     (M.margin_x_end 0) (M.margin_y_start 8) (M.margin_y_end 8)
     (M.word_in_bytes 4) (M.row_offset_in_bytes 8) (M.row_offset_in_words 2))
    |}]
;;
