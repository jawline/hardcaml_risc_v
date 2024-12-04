open! Core
open Hardcaml
open Hardcaml_risc_v
open Hardcaml_risc_v_hart
open Hardcaml_uart_controller
open Hardcaml_risc_v_test
open Hardcaml_waveterm
open Opcode_helper
open! Bits

let debug = true
let output_width = 64
let output_height = 35

let uart_config =
  { Hardcaml_uart_controller.Config.clock_frequency = 200
  ; baud_rate = 50
  ; include_parity_bit = true
  ; stop_bits = 1
  }
;;

module Uart_rx = Uart_rx.Make (struct
    let config = uart_config
  end)

module Uart_tx = Uart_tx.Make (struct
    let config = uart_config
  end)

module Cpu_with_dma_memory =
  System.Make
    (struct
      let register_width = Register_width.B32
      let num_registers = 32
    end)
    (struct
      let num_bytes = 65536
    end)
    (struct
      let num_harts = 1
      let include_io_controller = Io_controller_config.Uart_controller uart_config

      let include_video_out =
        Video_config.Video_out
          ( (module struct
              let output_width = 64
              let output_height = 64
              let input_width = 32
              let input_height = 32
              let framebuffer_address = 0x8000
            end : Video_out.Config)
          , (module struct
              (* TODO: Add a clock requirement *)

              let h_active = 64
              let v_active = 64
              let h_fp = 0
              let h_sync = 0
              let h_bp = 0
              let v_fp = 0
              let v_sync = 0
              let v_bp = 0
            end : Video_signals.Config) )
      ;;
    end)

module With_transmitter = struct
  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; data_in_valid : 'a
      ; data_in : 'a [@bits 8]
      }
    [@@deriving sexp_of, hardcaml ~rtlmangle:"$"]
  end

  module O = struct
    type 'a t =
      { data_out_valid : 'a
      ; data_out : 'a [@bits 8]
      ; pixel : 'a
      ; registers : 'a Cpu_with_dma_memory.Registers.t list [@length 1]
      }
    [@@deriving sexp_of, hardcaml ~rtlmangle:"$"]
  end

  let create scope { I.clock; clear; data_in_valid; data_in } =
    let open Signal in
    let { Uart_tx.O.uart_tx; _ } =
      Uart_tx.hierarchical
        ~instance:"tx"
        scope
        { Uart_tx.I.clock; clear; data_in_valid; data_in }
    in
    let { Cpu_with_dma_memory.O.registers; uart_tx = cpu_uart_tx; video_out; _ } =
      Cpu_with_dma_memory.hierarchical
        ~instance:"cpu"
        scope
        { clock; clear; uart_rx = Some uart_tx }
    in
    let { Uart_rx.O.data_out_valid; data_out; _ } =
      Uart_rx.hierarchical
        ~instance:"rx"
        scope
        { Uart_rx.I.clock; clear; uart_rx = Option.value_exn cpu_uart_tx }
    in
    let video_out = Option.value_exn video_out in
    { O.registers; data_out_valid; data_out; pixel = video_out.video_data.vdata.:(0) }
  ;;
end

type sim =
  (Bits.t ref With_transmitter.I.t, Bits.t ref With_transmitter.O.t) Cyclesim.t
  * Waveform.t
  * string

let create_sim name : sim =
  let module Sim = Cyclesim.With_interface (With_transmitter.I) (With_transmitter.O) in
  let sim =
    Sim.create
      ~config:Cyclesim.Config.trace_all
      (With_transmitter.create
         (Scope.create ~auto_label_hierarchical_ports:true ~flatten_design:true ()))
  in
  let waveform, sim = Waveform.create sim in
  sim, waveform, name
;;

let finalize_sim sim =
  if debug
  then (
    let _, waveform, name = sim in
    Waveform.Serialize.marshall waveform ("/tmp/programs_" ^ name))
  else ()
;;

let print_ram sim =
  let ram = Cyclesim.lookup_mem_by_name sim "main_memory_bram" |> Option.value_exn in
  let data =
    Cyclesim.Memory.read_all ram
    |> Array.to_list
    |> Bits.concat_lsb
    |> Bits.split_lsb ~part_width:8
    |> List.map ~f:Bits.to_char
    |> String.of_char_list
  in
  print_s [%message "" ~_:(data : String.Hexdump.t)]
;;

let clear_registers ~(inputs : Bits.t ref With_transmitter.I.t) sim =
  inputs.clear := Bits.one 1;
  Cyclesim.cycle sim;
  inputs.clear := Bits.zero 1
;;

let send_dma_message ~address ~packet sim =
  let inputs : _ With_transmitter.I.t = Cyclesim.inputs sim in
  let whole_packet = dma_packet ~address packet in
  (* Send the DMA message through byte by byte. Uart_tx will transmit a
     byte once every ~10 cycles (this is dependent on the number of stop
     bits and the parity bit. *)
  let rec loop_for n =
    if n = 0
    then ()
    else (
      Cyclesim.cycle sim;
      loop_for (n - 1))
  in
  List.iter
    ~f:(fun input ->
      inputs.data_in_valid := vdd;
      inputs.data_in := of_int ~width:8 input;
      Cyclesim.cycle sim;
      inputs.data_in_valid := of_int ~width:1 0;
      loop_for 44)
    whole_packet
;;

let which_cycle = ref 0
let which_px = ref 0
let frames = ref []
let current_frame = Array.init ~f:(fun _ -> false) (output_width * output_height)
let cycles_per_pixel = 20

let clear_video_state () =
  frames := [];
  which_cycle := 0;
  which_px := 0
;;

let consider_collecting_frame_buffer (outputs : Bits.t ref With_transmitter.O.t) =
  let write_framebuffer () =
    Array.set current_frame !which_px (Bits.to_bool !(outputs.pixel))
  in
  let emit_frame () = frames := Array.copy current_frame :: !frames in
  (* On a sampling cycle, store the pixel to the framebuffer. *)
  if !which_cycle % cycles_per_pixel = cycles_per_pixel - 1 then write_framebuffer ();
  (* On the last cycle, save the frame. *)
  if !which_cycle % cycles_per_pixel = 0 && !which_px = (output_width * output_height) - 1
  then emit_frame ()
;;

let test ~print_frames ~cycles ~data sim =
  let sim, _, _ = sim in
  let inputs : _ With_transmitter.I.t = Cyclesim.inputs sim in
  (* Initialize the main memory to some known values for testing. *)
  let initial_ram =
    Cyclesim.lookup_mem_by_name sim "main_memory_bram" |> Option.value_exn
  in
  Sequence.range 0 (Cyclesim.Memory.size_in_words initial_ram)
  |> Sequence.iter ~f:(fun i ->
    Cyclesim.Memory.of_bits ~address:i initial_ram (Bits.zero 32));
  (* Send a clear signal to initialize any CPU IO controller state back to
   * default so we're ready to receive. *)
  clear_registers ~inputs sim;
  send_dma_message ~address:0 ~packet:data sim;
  let _outputs_before : _ With_transmitter.O.t =
    Cyclesim.outputs ~clock_edge:Side.Before sim
  in
  let outputs : _ With_transmitter.O.t = Cyclesim.outputs sim in
  (* Wait some arbitrary number of cycles for the actual DMA to proceed. This is hard to guess, since the memory controller can push back. *)
  Sequence.range 0 100 |> Sequence.iter ~f:(fun _ -> Cyclesim.cycle sim);
  (* Send a clear signal and then start the vsync logic *)
  clear_registers ~inputs sim;
  clear_video_state ();
  print_s [%message "Printing RAM before registers"];
  let rec loop_for cycles =
    if cycles = 0
    then ()
    else (
      Cyclesim.cycle sim;
      consider_collecting_frame_buffer outputs;
      incr which_cycle;
      if Bits.to_bool !(outputs.data_out_valid)
      then printf "%c" (Bits.to_char !(outputs.data_out));
      loop_for (cycles - 1))
  in
  printf "RECEIVED FROM CPU VIA DMA: ";
  loop_for cycles;
  printf "\n";
  if print_frames
  then
    List.iteri
      ~f:(fun idx frame ->
        printf "Framebuffer %i\n" idx;
        Sequence.range 0 output_height
        |> Sequence.iter ~f:(fun y ->
          Sequence.range 0 output_width
          |> Sequence.iter ~f:(fun x ->
            let px = Array.get frame ((y * output_width) + x) in
            printf "%s" (if px then "*" else "-"));
          printf "\n"))
      (* Frames are in reverse order because we're prepending. *)
      (List.rev !frames);
  match outputs.registers with
  | [ outputs ] ->
    let outputs =
      Cpu_with_dma_memory.Registers.map ~f:(fun t -> Bits.to_int !t) outputs
    in
    print_s [%message "" ~_:(outputs : int Cpu_with_dma_memory.Registers.t)];
    print_ram sim
  | _ -> raise_s [%message "BUG: Unexpected number of harts"]
;;

let%expect_test "Hello world" =
  let program = In_channel.read_all "../test-programs/hello_world_c/hello_world" in
  let sim = create_sim "test_dma_hello_world" in
  test ~print_frames:false ~cycles:5000 ~data:program sim;
  finalize_sim sim;
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)
  ("Failed to perform unsigned integer conversion on dotted operator"
    (op_name equals)
    (conversion_error ("[of_unsigned_int] input value is less than 0" (x -1))))
  Raised at Base__Error.raise in file "src/error.ml", line 9, characters 72-88
  Called from Hardcaml__Comb.Make.op_int_right in file "src/comb.ml" (inlined), line 529, characters 4-45
  Called from Hardcaml_risc_v__Video_signals.Make.create in file "lib/video_signals.ml", line 51, characters 46-72
  Called from Hardcaml__Hierarchy.In_scope.create in file "src/hierarchy.ml", line 105, characters 18-40
  Called from Hardcaml_risc_v__Video_out.Make.create in file "lib/video_out.ml", lines 47-49, characters 6-60
  Called from Hardcaml__Hierarchy.In_scope.create in file "src/hierarchy.ml", line 105, characters 18-40
  Called from Hardcaml_risc_v__System.Make.maybe_video_out in file "lib/system.ml", lines 229-237, characters 8-11
  Called from Hardcaml_risc_v__System.Make.create in file "lib/system.ml", line 249, characters 26-49
  Called from Hardcaml__Hierarchy.In_scope.create in file "src/hierarchy.ml", line 105, characters 18-40
  Called from Hardcaml_risc_v_regression_tests__Test_programs.With_transmitter.create in file "regression-tests/test_programs.ml", lines 98-101, characters 6-48
  Called from Hardcaml__Circuit.With_interface.create_exn in file "src/circuit.ml", line 424, characters 18-34
  Called from Hardcaml__Cyclesim.With_interface.create in file "src/cyclesim.ml", line 146, characters 18-81
  Called from Hardcaml_risc_v_regression_tests__Test_programs.create_sim in file "regression-tests/test_programs.ml", lines 122-125, characters 4-84
  Called from Hardcaml_risc_v_regression_tests__Test_programs.(fun) in file "regression-tests/test_programs.ml", line 269, characters 12-45
  Called from Ppx_expect_runtime__Test_block.Configured.dump_backtrace in file "runtime/test_block.ml", line 142, characters 10-28
  |}]
;;

let%expect_test "Game of life" =
  let program = In_channel.read_all "../test-programs/game_of_life/game_of_life" in
  let sim = create_sim "test_dma_game_of_life" in
  test ~print_frames:true ~cycles:500_000 ~data:program sim;
  finalize_sim sim;
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)
  ("Failed to perform unsigned integer conversion on dotted operator"
    (op_name equals)
    (conversion_error ("[of_unsigned_int] input value is less than 0" (x -1))))
  Raised at Base__Error.raise in file "src/error.ml", line 9, characters 72-88
  Called from Hardcaml__Comb.Make.op_int_right in file "src/comb.ml" (inlined), line 529, characters 4-45
  Called from Hardcaml_risc_v__Video_signals.Make.create in file "lib/video_signals.ml", line 51, characters 46-72
  Called from Hardcaml__Hierarchy.In_scope.create in file "src/hierarchy.ml", line 105, characters 18-40
  Called from Hardcaml_risc_v__Video_out.Make.create in file "lib/video_out.ml", lines 47-49, characters 6-60
  Called from Hardcaml__Hierarchy.In_scope.create in file "src/hierarchy.ml", line 105, characters 18-40
  Called from Hardcaml_risc_v__System.Make.maybe_video_out in file "lib/system.ml", lines 229-237, characters 8-11
  Called from Hardcaml_risc_v__System.Make.create in file "lib/system.ml", line 249, characters 26-49
  Called from Hardcaml__Hierarchy.In_scope.create in file "src/hierarchy.ml", line 105, characters 18-40
  Called from Hardcaml_risc_v_regression_tests__Test_programs.With_transmitter.create in file "regression-tests/test_programs.ml", lines 98-101, characters 6-48
  Called from Hardcaml__Circuit.With_interface.create_exn in file "src/circuit.ml", line 424, characters 18-34
  Called from Hardcaml__Cyclesim.With_interface.create in file "src/cyclesim.ml", line 146, characters 18-81
  Called from Hardcaml_risc_v_regression_tests__Test_programs.create_sim in file "regression-tests/test_programs.ml", lines 122-125, characters 4-84
  Called from Hardcaml_risc_v_regression_tests__Test_programs.(fun) in file "regression-tests/test_programs.ml", line 303, characters 12-46
  Called from Ppx_expect_runtime__Test_block.Configured.dump_backtrace in file "runtime/test_block.ml", line 142, characters 10-28
  |}]
;;
