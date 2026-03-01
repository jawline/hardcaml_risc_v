(** A framebuffer expander that reads a framebuffer of input_width and
    input_height from the memory controller and outputs it pixel by pixel width
    by height.

    At the start of the frame start should be called, providing some cycles of
    leeway before the first pixel is necessary for prefetching.

    To move on to the next pixel the next input should be set high for a cycle.
    As with start, the next pixel should be sampled some cycles after the next
    pulse to provide leeway for fetching.

    Row memory must be word aligned (e.g, if a row is 8 width it must still be
    4 bytes long).

    There are three modes which change the representation of a framebuffer in
    memory:
    - One_bit:  A single bit is used to represent the pixel.
    - Grescale_8bit: Pixels are represented as a densely packed array of 8 bit greyscale values. 8 bits per pixel.
    - RGB_8bit: Pixels are represented by 24bit RGB values, this mode requires the highest memory bandwidth.

    If memory is not available in time, the output is not well defined.
*)

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
       val input_pixel_mode : Pixel_mode.t
     end)
    (Memory : Memory_bus_intf.S) =
struct
  open Config

  let data_width = Memory.data_bus_width

  module Pixel = struct
    type 'a t =
      { r : 'a [@bits 8]
      ; g : 'a [@bits 8]
      ; b : 'a [@bits 8]
      }
    [@@deriving hardcaml]
  end

  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; start : 'a
      ; next : 'a
      ; start_address : 'a [@bits Memory.Read_bus.Source.port_widths.data.address]
      ; memory_request : 'a Memory.Read_bus.Dest.t
      ; memory_response : 'a Memory.Read_response.With_valid.t
      }
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t =
      { valid : 'a
      ; pixel : 'a Pixel.t
      ; memory_request : 'a Memory.Read_bus.Source.t
      }
    [@@deriving hardcaml]
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
    [@@deriving compare ~localize, enumerate, sexp_of]
  end

  (* We require that row memory be word aligned (e.g, even if row width % 4 <>
     0 we still say the row is a multiple of 4 bytes wide).

     This greatly simplifies thinking about framebuffer row offsets since
     bitvectors are always word aligned. *)

  let word_in_bytes = I.port_widths.memory_response.value.read_data / 8

  let bits_per_pixel =
    match Config.input_pixel_mode with
    | One_bit -> 1
    | Greyscale_8bit -> 8
    | RGB_8bit -> 24
  ;;

  let bits_per_row = input_width * bits_per_pixel

  (* Number of bytes in a row, aligned to the nearest byte (8 bits). *)
  let bytes_per_row =
    let whole_component = bits_per_row / 8 in
    let ratio_component = bits_per_row % 8 in
    whole_component + if ratio_component <> 0 then 1 else 0
  ;;

  let row_offset_in_words =
    let whole_component = bytes_per_row / word_in_bytes in
    let ratio_component = bytes_per_row % word_in_bytes in
    whole_component + if ratio_component <> 0 then 1 else 0
  ;;

  (* In RGB mode we might have some pixels that don't align with read widths. E.g.,  
     
       32 bits
       [ R; G; B; R ]  [ G ; B; R ; G ] [ B; R; G: B ] -> 1 bit misaligned

       Pixel 0: RGB, 1 byte rem
       Pixel 1: RGB, 2 byte rem
       Pixel 2: RGB, 3 byte rem

       pixels_this_mem_cell = [ 1; 1; 2 ] 

       0x0: RG BR GB RG BR GB RG BR GB

       Scheme to handle this:

       Previously: We would advance the read head when a fixed constant pixel this mem cell was full
       Now: We will advance the head when a dynamic value pixels_this_mem_cell is full 
       where pixels this mem cell is driven by a precomputed counter

       Compute

       cycle: first repeat in num bytes rem
       so 32b: 3 mem cell cycle
       pixel 0 (1 rem, pixels this mem cell = 1) -> pixel 1 (2 rem, pixels this mem cell = 1) -> pixel 2 (3 rem, pixels this mem cell = 2)

       reg width = 24 bits + (max rem * 8, 24 bits at a 32bit bus)

       pixels_this_cell = look up table of how many pixels to emit this cell

       on data cell: data <- data.value lsl 3 |: memory cell lsr 3

       on ready:
        if cur_pixel_this_mem_cell = pixels_this_mem_cell then schedule next read else 

       output data: sll data ~by:(cur_pixel_this_mem_cell * 24)

     *)
  let min_pixels_every_memory_cell = data_width / bits_per_pixel
  let rem_bytes_incr_per_memory_cell = data_width % bits_per_pixel

  let num_pixels_and_remaining_per_memory_cell =
    match Config.input_pixel_mode with
    | One_bit | Greyscale_8bit -> [ 0, min_pixels_every_memory_cell ]
    | RGB_8bit ->
      let bytes_per_pixel = bits_per_pixel / 8 in
      (* Find a loop of remainders from which we build our cycle schedule *)
      let rec compute_rem_cycles prev_entries =
        let prev_rem = List.hd_exn prev_entries |> fst in
        let rem_this_cycle =
          (prev_rem + rem_bytes_incr_per_memory_cell) % bytes_per_pixel
        in
        let pixels_this_cycle =
          if prev_rem + rem_bytes_incr_per_memory_cell >= bytes_per_pixel
          then min_pixels_every_memory_cell + 1
          else min_pixels_every_memory_cell
        in
        let rem_exists =
          List.find ~f:(fun (rem, _) -> rem = rem_this_cycle) prev_entries
          |> Option.is_some
        in
        if rem_exists
        then List.rev prev_entries
        else compute_rem_cycles ((rem_this_cycle, pixels_this_cycle) :: prev_entries)
      in
      compute_rem_cycles [ rem_bytes_incr_per_memory_cell, min_pixels_every_memory_cell ]
  ;;

  let num_pixels_per_memory_cell =
    num_pixels_and_remaining_per_memory_cell |> List.map ~f:snd
  ;;

  let max_num_pixels_per_memory_cell =
    match Config.input_pixel_mode with
    | One_bit -> data_width
    | Greyscale_8bit -> data_width / 8
    | RGB_8bit -> List.reduce_exn ~f:Int.max num_pixels_per_memory_cell
  ;;

  let last_pixel_for_memory_cell t =
    List.map
      ~f:(fun t ->
        of_int_trunc ~width:(address_bits_for max_num_pixels_per_memory_cell) (t - 1))
      num_pixels_per_memory_cell
    |> mux t
  ;;

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
    let%hw.State_machine current_state = State_machine.create (module State) reg_spec in
    let%hw_var data =
      Variable.reg
        ~width:(max_num_pixels_per_memory_cell * bits_per_pixel)
        reg_spec_no_clear
    in
    let%hw_var cycle_schedule =
      Variable.reg
        ~width:(address_bits_for (List.length num_pixels_per_memory_cell))
        reg_spec_no_clear
    in
    let%hw_var last_pixel_this_memory_cell =
      Variable.reg
        ~width:(address_bits_for max_num_pixels_per_memory_cell)
        reg_spec_no_clear
    in
    let%hw next_cycle_schedule =
      mux2
        (cycle_schedule.value ==:. List.length num_pixels_per_memory_cell - 1)
        (zero (width cycle_schedule.value))
        (cycle_schedule.value +:. 1)
    in
    let%hw last_pixel_first_memory_cell =
      last_pixel_for_memory_cell (of_int_trunc ~width:(width cycle_schedule.value) 0)
    in
    let%hw_var which_pixel_this_memory_cell =
      Variable.reg
        ~width:(address_bits_for max_num_pixels_per_memory_cell)
        reg_spec_no_clear
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
      let aligned_address =
        (Memory.byte_address_to_memory_address i.start_address).value
      in
      proc
        [ reg_x <--. 0
        ; reg_y <--. 0
        ; x_px_ctr <--. 0
        ; y_line_ctr <--. 0
        ; y_px_ctr <--. 0
        ; next_address <-- aligned_address
        ; row_start_address <-- aligned_address
        ; cycle_schedule <--. 0
        ; last_pixel_this_memory_cell <-- last_pixel_first_memory_cell
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
        reg reg_spec_no_clear (row_start_address.value +:. row_offset_in_words)
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
        ; cycle_schedule <--. 0
        ; which_pixel_this_memory_cell <--. 0
        ; last_pixel_this_memory_cell <-- last_pixel_first_memory_cell
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
            ; incr which_pixel_this_memory_cell
            ; when_
                (which_pixel_this_memory_cell.value ==: last_pixel_this_memory_cell.value)
                [ incr next_address
                ; which_pixel_this_memory_cell <--. 0
                ; cycle_schedule <-- next_cycle_schedule
                ; last_pixel_this_memory_cell
                  <-- last_pixel_for_memory_cell next_cycle_schedule
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
    let%hw memory_data_including_trailing =
      match Config.input_pixel_mode with
      | One_bit | Greyscale_8bit ->
        (* Byte aligned pixels do not need trailing data. *)
        i.memory_response.value.read_data
      | RGB_8bit ->
        (* in RGB mode we use 24 bits per pixel. This means that we don't use some trailing data for memory reads > 3. *)
        mux_init
          ~f:(fun which_cycle ->
            match which_cycle with
            | 0 -> uextend ~width:(width data.value) i.memory_response.value.read_data
            | n ->
              let pixels_last_cell = List.nth_exn num_pixels_per_memory_cell (n - 1) in
              let remaining_from_last_cell =
                drop_bottom ~width:(pixels_last_cell * 8) data.value
              in
              concat_lsb [ remaining_from_last_cell; i.memory_response.value.read_data ]
              |> uextend ~width:(width data.value))
          cycle_schedule.value
          (List.length num_pixels_per_memory_cell)
    in
    compile
      [ when_ i.next [ proceed ]
      ; when_ i.start [ start ]
      ; when_ (request_read &: i.memory_request.ready) [ fetched <-- vdd ]
      ; when_
          i.memory_response.valid
          [ data_valid <-- vdd; data <-- memory_data_including_trailing ]
      ];
    let body_pixel =
      match Config.input_pixel_mode with
      | One_bit ->
        let bit =
          mux_init
            ~f:(fun i -> bit ~pos:i data.value)
            which_pixel_this_memory_cell.value
            (width data.value)
        in
        let bit_rval = repeat ~count:8 bit in
        { Pixel.r = bit_rval; g = bit_rval; b = bit_rval }
      | Greyscale_8bit ->
        let pixel =
          mux_init
            ~f:(fun i -> drop_bottom ~width:(i * 8) data.value |> sel_bottom ~width:8)
            which_pixel_this_memory_cell.value
            max_num_pixels_per_memory_cell
        in
        { Pixel.r = pixel; g = pixel; b = pixel }
      | RGB_8bit -> assert false
    in
    { O.valid =
        ~:(current_state.is X_body) |: (current_state.is X_body &: data_valid.value)
    ; pixel = Pixel.Of_signal.(mux2 (current_state.is X_body) body_pixel (zero ()))
    ; memory_request =
        { Memory.Read_bus.Source.valid = request_read
        ; data = { address = next_address.value }
        }
    }
  ;;

  let hierarchical (scope : Scope.t) (input : Signal.t I.t) =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ~scope ~name:"framebuffer_expander" create input
  ;;
end

let%expect_test "pixel test" =
  let module Memory_controller =
    Bram_memory_controller.Make (struct
      let capacity_in_bytes = 256
      let num_write_channels = 1
      let num_read_channels = 1
      let address_width = 32
      let data_bus_width = 32
    end)
  in
  let print_config
        ~input_width
        ~input_height
        ~output_width
        ~output_height
        ~input_pixel_mode
    =
    let module M =
      Make
        (struct
          let input_width = input_width
          let input_height = input_height
          let output_width = output_width
          let output_height = output_height
          let input_pixel_mode = input_pixel_mode
        end)
        (Memory_controller.Memory_bus)
    in
    print_s
      [%message
        (input_width : int)
          (input_height : int)
          (M.bits_per_pixel : int)
          (M.bits_per_row : int)
          (M.bytes_per_row : int)]
  in
  print_config
    ~input_width:8
    ~input_height:16
    ~output_width:100
    ~output_height:200
    ~input_pixel_mode:Pixel_mode.One_bit;
  print_config
    ~input_width:8
    ~input_height:16
    ~output_width:100
    ~output_height:200
    ~input_pixel_mode:Pixel_mode.Greyscale_8bit;
  print_config
    ~input_width:8
    ~input_height:16
    ~output_width:100
    ~output_height:200
    ~input_pixel_mode:Pixel_mode.RGB_8bit;
  [%expect
    {|
    ((input_width 8) (input_height 16) (M.bits_per_pixel 1) (M.bits_per_row 8)
     (M.bytes_per_row 1))
    ((input_width 8) (input_height 16) (M.bits_per_pixel 8) (M.bits_per_row 64)
     (M.bytes_per_row 8))
    ((input_width 8) (input_height 16) (M.bits_per_pixel 24) (M.bits_per_row 192)
     (M.bytes_per_row 24))
    |}]
;;

let%expect_test "margins and scaling factor tests" =
  let module Memory_controller =
    Bram_memory_controller.Make (struct
      let capacity_in_bytes = 256
      let num_write_channels = 1
      let num_read_channels = 1
      let address_width = 32
      let data_bus_width = 32
    end)
  in
  let print_config
        ~input_width
        ~input_height
        ~output_width
        ~output_height
        ~input_pixel_mode
    =
    let module M =
      Make
        (struct
          let input_width = input_width
          let input_height = input_height
          let output_width = output_width
          let output_height = output_height
          let input_pixel_mode = input_pixel_mode
        end)
        (Memory_controller.Memory_bus)
    in
    print_s
      [%message
        (input_width : int)
          (input_height : int)
          (output_width : int)
          (output_height : int)
          (input_pixel_mode : Pixel_mode.t)
          (M.scaling_factor_x : int)
          (M.scaling_factor_y : int)
          (M.margin_x_start : int)
          (M.margin_x_end : int)
          (M.margin_y_start : int)
          (M.margin_y_end : int)
          (M.word_in_bytes : int)
          (M.row_offset_in_words : int)]
  in
  print_config
    ~input_width:3
    ~input_height:3
    ~output_width:133
    ~output_height:35
    ~input_pixel_mode:Pixel_mode.One_bit;
  print_config
    ~input_width:64
    ~input_height:32
    ~output_width:1280
    ~output_height:720
    ~input_pixel_mode:Pixel_mode.One_bit;
  [%expect
    {|
    ((input_width 3) (input_height 3) (output_width 133) (output_height 35)
     (input_pixel_mode One_bit) (M.scaling_factor_x 44) (M.scaling_factor_y 11)
     (M.margin_x_start 1) (M.margin_x_end 0) (M.margin_y_start 1)
     (M.margin_y_end 1) (M.word_in_bytes 4) (M.row_offset_in_words 1))
    ((input_width 64) (input_height 32) (output_width 1280) (output_height 720)
     (input_pixel_mode One_bit) (M.scaling_factor_x 20) (M.scaling_factor_y 22)
     (M.margin_x_start 0) (M.margin_x_end 0) (M.margin_y_start 8)
     (M.margin_y_end 8) (M.word_in_bytes 4) (M.row_offset_in_words 2))
    |}]
;;
