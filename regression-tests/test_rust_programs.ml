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
              let h_fp = 1000
              let h_sync = 10
              let h_bp = 1000
              let v_fp = 1
              let v_sync = 1
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

let%expect_test "Hello world (Rust)" =
  let program =
    In_channel.read_all "../test-programs/hello-world-rust/hello-world-rust.bin"
  in
  let sim = create_sim "test_hello_world_rust" in
  test ~print_frames:false ~cycles:5000 ~data:program sim;
  finalize_sim sim;
  [%expect
    {|
    "Printing RAM before registers"
    RECEIVED FROM CPU VIA DMA: D HELLO WORLDD AND NOW GOODBYE
    ((pc 240)
     (general
      (0 236 16368 0 0 1 255 15 255 0 1 255 15 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0)))
    ("00000000  93 00 00 00 13 01 00 00  93 01 00 00 13 02 00 00  |................|"
     "00000010  93 02 00 00 13 03 00 00  93 03 00 00 13 04 00 00  |................|"
     "00000020  93 04 00 00 13 05 00 00  93 05 00 00 13 06 00 00  |................|"
     "00000030  93 06 00 00 13 07 00 00  93 07 00 00 13 08 00 00  |................|"
     "00000040  93 08 00 00 13 09 00 00  93 09 00 00 13 0a 00 00  |................|"
     "00000050  93 0a 00 00 13 0b 00 00  93 0b 00 00 13 0c 00 00  |................|"
     "00000060  93 0c 00 00 13 0d 00 00  93 0d 00 00 13 0e 00 00  |................|"
     "00000070  93 0e 00 00 13 0f 00 00  93 0f 00 00 17 41 00 00  |.............A..|"
     "00000080  13 01 41 f8 ef 00 00 02  6f 00 00 00 93 02 05 00  |..A.....o.......|"
     "00000090  13 83 05 00 93 03 06 00  73 00 00 00 13 85 02 00  |........s.......|"
     "000000a0  67 80 00 00 13 01 01 ff  23 26 11 00 23 24 81 00  |g.......#&..#$..|"
     "000000b0  37 05 00 00 13 04 45 0f  13 06 b0 00 13 05 00 00  |7.....E.........|"
     "000000c0  93 05 04 00 97 00 00 00  e7 80 80 fc e3 06 05 fe  |................|"
     "000000d0  37 05 00 00 13 04 f5 0f  13 06 f0 00 13 05 00 00  |7...............|"
     "000000e0  93 05 04 00 97 00 00 00  e7 80 80 fa e3 06 05 fe  |................|"
     "000000f0  6f 00 00 00 48 45 4c 4c  4f 20 57 4f 52 4c 44 41  |o...HELLO WORLDA|"
     "00000100  4e 44 20 4e 4f 57 20 47  4f 4f 44 42 59 45 44 42  |ND NOW GOODBYEDB|"
     "00000110  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "00000120  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "00000130  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "00000140  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "00000150  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "00000160  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "00000170  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "00000180  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "00000190  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "000001a0  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "000001b0  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "000001c0  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "000001d0  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "000001e0  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "000001f0  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "00000200  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "00000210  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "00000220  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "00000230  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "00000240  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "00000250  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "00000260  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "00000270  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "00000280  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "00000290  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "000002a0  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "000002b0  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "000002c0  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "000002d0  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "000002e0  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "000002f0  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "00000300  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "00000310  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "00000320  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "00000330  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "00000340  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "00000350  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "00000360  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "00000370  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "00000380  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "00000390  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "000003a0  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "000003b0  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "000003c0  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "000003d0  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "000003e0  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "000003f0  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "00000400  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "00000410  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "00000420  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "00000430  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "00000440  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "00000450  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "00000460  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "00000470  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "00000480  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "00000490  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "000004a0  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "000004b0  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "000004c0  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "000004d0  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "000004e0  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "000004f0  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "00000500  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "00000510  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "00000520  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "00000530  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "00000540  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "00000550  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "00000560  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "00000570  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "00000580  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "00000590  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "000005a0  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "000005b0  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "000005c0  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "000005d0  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "000005e0  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "000005f0  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "00000600  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "00000610  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "00000620  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "00000630  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "00000640  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "00000650  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "00000660  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "00000670  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "00000680  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "00000690  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "000006a0  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "000006b0  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "000006c0  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "000006d0  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "000006e0  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "000006f0  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "00000700  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "00000710  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "00000720  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "00000730  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "00000740  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "00000750  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "00000760  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "00000770  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "00000780  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "00000790  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "000007a0  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "000007b0  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "000007c0  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "000007d0  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "000007e0  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "000007f0  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     ...
     "0000f800  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000f810  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000f820  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000f830  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000f840  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000f850  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000f860  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000f870  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000f880  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000f890  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000f8a0  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000f8b0  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000f8c0  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000f8d0  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000f8e0  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000f8f0  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000f900  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000f910  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000f920  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000f930  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000f940  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000f950  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000f960  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000f970  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000f980  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000f990  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000f9a0  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000f9b0  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000f9c0  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000f9d0  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000f9e0  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000f9f0  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000fa00  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000fa10  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000fa20  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000fa30  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000fa40  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000fa50  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000fa60  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000fa70  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000fa80  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000fa90  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000faa0  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000fab0  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000fac0  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000fad0  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000fae0  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000faf0  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000fb00  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000fb10  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000fb20  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000fb30  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000fb40  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000fb50  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000fb60  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000fb70  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000fb80  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000fb90  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000fba0  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000fbb0  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000fbc0  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000fbd0  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000fbe0  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000fbf0  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000fc00  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000fc10  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000fc20  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000fc30  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000fc40  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000fc50  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000fc60  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000fc70  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000fc80  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000fc90  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000fca0  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000fcb0  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000fcc0  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000fcd0  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000fce0  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000fcf0  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000fd00  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000fd10  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000fd20  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000fd30  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000fd40  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000fd50  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000fd60  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000fd70  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000fd80  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000fd90  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000fda0  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000fdb0  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000fdc0  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000fdd0  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000fde0  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000fdf0  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000fe00  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000fe10  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000fe20  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000fe30  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000fe40  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000fe50  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000fe60  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000fe70  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000fe80  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000fe90  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000fea0  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000feb0  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000fec0  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000fed0  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000fee0  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000fef0  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000ff00  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000ff10  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000ff20  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000ff30  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000ff40  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000ff50  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000ff60  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000ff70  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000ff80  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000ff90  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000ffa0  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000ffb0  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000ffc0  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000ffd0  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000ffe0  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000fff0  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|")
    |}]
;;


let%expect_test "Chip-8 (Rust)" =
  let program =
    In_channel.read_all "../test-programs/chip-8-rust/chip-8-rust.bin"
  in
  let sim = create_sim "test_chip_8_rust" in
  test ~print_frames:false ~cycles:1_000_000 ~data:program sim;
  finalize_sim sim;
  [%expect
    {|
    "Printing RAM before registers"
    RECEIVED FROM CPU VIA DMA: D Starting upD Transmitting program locationD 12417D Got handle on programD Initialized framebufferD Initializing op tablesD Initialized op tablesD Initialized, steppingD StepD Transmit offsetD 512D Transmit offsetD 513D StepD Transmit offsetD 514D Transmit offsetD 515D StepD Transmit offsetD 516D Transmit offsetD 517D StepD Transmit offsetD 518D Transmit offsetD 519D StepD Transmit offsetD 520D Transmit offsetD 521D StepD Transmit offsetD 522D Transmit offsetD 523D Transmit offsetD 746D Transmit offsetD 747D Transmit offsetD 748D Transmit offsetD 749D Transmit offsetD 750D Transmit offsetD 751D StepD Transmit offsetD 524D Transmit offsetD 525D Transmit offsetD 746D Transmit offsetD 747D Transmit offsetD 748D Transmit offsetD 749D Transmit offsetD 750D Transmit offsetD 751D StepD Transmit offsetD 526D Transmit offsetD 527D StepD Transmit offsetD 528D Transmit offsetD 529D 
    PUSH STACKD StepD Transmit offsetD 724D Transmit offsetD 725D StepD Transmit offsetD 726D Transmit offsetD 727D StepD Transmit offsetD 728D Transmit offsetD 729D Transmit offsetD 757D Transmit offsetD 758D Transmit offsetD 759D StepD Transmit offsetD 730D Transmit offsetD 731D StepD Transmit offsetD 732D Transmit offsetD 733D StepD Transmit offsetD 734D Transmit offsetD 735D StepD Transmit offsetD 736D Transmit offsetD 737D Transmit offsetD 16384D Transmit offsetD 16385D Transmit offsetD 16386D Transmit offsetD 16387D Transmit offsetD 16388D StepD Transmit offsetD 738D Transmit offsetD 739D StepD Transmit offsetD 740D Transmit offsetD 741D StepD Transmit offsetD 742D Transmit offsetD 743D Transmit offsetD 16384D Transmit offsetD 16385D Transmit offsetD 16386D Transmit offsetD 16387D Transmit offsetD 16388D StepD Transmit offsetD 744D Transmit offsetD 745D 	POP STACKD StepD Transmit offsetD 530D Transmit offsetD 531D StepD Transmit offsetD 532D Transmit offsetD 533D StepD Transmit offsetD 534D Transmit offsetD 535D StepD Transmit offsetD 536D Transmit offsetD 537D StepD Transmit offsetD 538D Transmit offsetD 539D StepD Transmit offsetD 540D Transmit offsetD 541D StepD Transmit offsetD 542D Transmit offsetD 543D StepD Transmit offsetD 538D Transmit offsetD 539D StepD Transmit offsetD 540D Transmit offsetD 541D StepD Transmit offsetD 542D Transmit offsetD 543D StepD Transmit offsetD 538D Transmit offsetD 539D StepD Transmit offsetD 540D Transmit offsetD 541D StepD Transmit offsetD 542D Transmit offsetD 543D StepD Transmit offsetD 538D Transmit offsetD 539D StepD Transmit offsetD 540D Transmit offsetD 541D StepD Transmit offsetD 542D Transmit offsetD 543D StepD Transmit offsetD 538D Transmit offsetD 539D StepD Transmit offsetD 540D Transmit offsetD 541D StepD Transmit offsetD 542D Transmit offsetD 543D StepD Transmit offsetD 538D Transmit offsetD 539D StepD Transmit offsetD 540D Transmit offsetD 541D StepD Transmit offsetD 542D Transmit offsetD 543D StepD Transmit offsetD 538D Transmit offsetD 539D StepD Transmit offsetD 540D Transmit offsetD 541D StepD Transmit offsetD 542D Transmit offsetD 543D StepD Transmit offsetD 538D Transmit offsetD 539D StepD Transmit offsetD 540D Transmit offsetD 541D StepD Transmit offsetD 542D Transmit offsetD 543D StepD Transmit offsetD 538D Transmit offsetD 539D StepD Transmit offsetD 540D Transmit offsetD 541D StepD Transmit offsetD 542D Transmit offsetD 543D StepD Transmit offsetD 538D Transmit offsetD 539D StepD Transmit offsetD 540D Transmit offsetD 541D StepD Transmit offsetD 542D Transmit offsetD 543D StepD Transmit offsetD 538D Transmit offsetD 539D StepD Transmit offsetD 540D Transmit offsetD 541D StepD Transmit offsetD 542D Transmit offsetD 543D StepD Transmit offsetD 538D Transmit offsetD 539D StepD Transmit offsetD 540D Transmit offsetD 541D StepD Transmit offsetD 542D Transmit offsetD 543D StepD Transmit offsetD 538D Transmit offsetD 539D StepD Transmit offsetD 540D Transmit offsetD 541D StepD Transmit offsetD 542D Transmit offsetD 543D StepD Transmit offsetD 538D Transmit offsetD 539D StepD Transmit offsetD 540D Transmit offsetD 541D StepD Transmit offsetD 542D Transmit offsetD 543D StepD Transmit offsetD 538D Transmit offsetD 539D StepD Transmit offsetD 540D Transmit offsetD 541D StepD Transmit offsetD 542D Transmit offsetD 543D StepD Transmit offsetD 538D Transmit offsetD 539D StepD Transmit offsetD 540D Transmit offsetD 541D StepD Transmit offsetD 542D Transmit offsetD 543D StepD Transmit offsetD 538D Transmit offsetD 539D StepD Transmit offsetD 540D Transmit offsetD 541D StepD Transmit offsetD 542D Transmit offsetD 543D StepD Transmit offsetD 538D Transmit offsetD 539D StepD Transmit offsetD 540D Transmit offsetD 541D StepD Transmit offsetD 542D Transmit offsetD 543D StepD Transmit offsetD 538D Transmit offsetD 539D StepD Transmit offsetD 540D Transmit offsetD 541D StepD Transmit offsetD 542D Transmit offsetD 543D StepD Transmit offsetD 538D Transmit offsetD 539D StepD Transmit offsetD 540D Transmit offsetD 541D StepD Transmit offsetD 542D Transmit offsetD 543D StepD Transmit offsetD 538D Transmit offsetD 539D StepD Transmit offsetD 540D Transmit offsetD 541D StepD Transmit offsetD 542D Transmit offsetD 543D StepD Transmit offsetD 538D Transmit offsetD 539D StepD Transmit offsetD 540D Transmit offsetD 541D StepD Transmit offsetD 542D Transmit offsetD 543D StepD Transmit offsetD 538D Transmit offsetD 539D StepD Transmit offsetD 540D Transmit offsetD 541D StepD Transmit offsetD 542D Transmit offsetD 543D StepD Transmit offsetD 538D Transmit offsetD 539D StepD Transmit offsetD 540D Transmit offsetD 541D StepD Transmit offsetD 542D Transmit offsetD 543D StepD Transmit offsetD 538D Transmit offsetD 539D StepD Transmit offsetD 540D Transmit offsetD 541D StepD Transmit offsetD 542D Transmit offsetD 543D StepD Transmit offsetD 538D Transmit offsetD 539D StepD Transmit offsetD 540D Transmit offsetD 541D StepD Transmit offsetD 542D Transmit offsetD 543D StepD Transmit offsetD 538D Transmit offsetD 539D StepD Transmit offsetD 540D Transmit offsetD 541D StepD Transmit offsetD 542D Transmit offsetD 543D StepD Transmit offsetD 538D Transmit offsetD 539D StepD Transmit offsetD 540D Transmit offsetD 541D StepD Transmit offsetD 542D Transmit offsetD 543D StepD Transmit offsetD 538D Transmit offsetD 539D StepD Transmit offsetD 540D Transmit offsetD 541D StepD Transmit offsetD 542D Transmit offsetD 543D StepD Transmit offsetD 538D Transmit offsetD 539D StepD Transmit offsetD 540D Transmit offsetD 541D StepD Transmit offsetD 542D Transmit offsetD 543D StepD Transmit offsetD 538D Transmit offsetD 539D StepD Transmit offsetD 540D Transmit offsetD 541D StepD Transmit offsetD 542D Transmit offsetD 543D StepD Transmit offsetD 538D Transmit offsetD 539D StepD Transmit offsetD 540D Transmit offsetD 541D StepD Transmit offsetD 542D Transmit offsetD 543D StepD Transmit offsetD 538D Transmit offsetD 539D StepD Transmit offsetD 540D Transmit offsetD 541D StepD Transmit offsetD 544D Transmit offsetD 545D StepD Transmit offsetD 546D Transmit offsetD 547D StepD Transmit offsetD 548D Transmit offsetD 549D StepD Transmit offsetD 550D Transmit offsetD 551D StepD Transmit offsetD 552D Transmit offsetD 553D Transmit offsetD 752D StepD Transmit offsetD 554D Transmit offsetD 555D StepD Transmit offsetD 556D Transmit offsetD 557D Transmit offsetD 746D Transmit offsetD 747D Transmit offsetD 748D Transmit offsetD 749D Transmit offsetD 750D Transmit offsetD 751D StepD Transmit offsetD 558D Transmit offsetD 559D Transmit offsetD 746D Transmit offsetD 747D Transmit offsetD 748D Transmit offsetD 749D Transmit offsetD 750D Transmit offsetD 751D StepD Transmit offsetD 560D Transmit offsetD 561D StepD Transmit offsetD 562D Transmit offsetD 563D StepD Transmit offsetD 566D Transmit offsetD 567D StepD Transmit offsetD 568D Transmit offsetD 569D StepD Transmit offsetD 572D Transmit offsetD 573D StepD Transmit offsetD 574D Transmit offsetD 575D StepD Transmit offsetD 576D Transmit offsetD 577D Transmit offsetD 746D Transmit offsetD 747D Transmit offsetD 748D Transmit offsetD 749D Transmit offsetD 750D Transmit offsetD 751D StepD Transmit offsetD 578D Transmit offsetD 579D StepD Transmit offsetD 580D Transmit offsetD 581D StepD Transmit offsetD 582D Transmit offsetD 583D StepD Transmit offsetD 584D Transmit offsetD 585D StepD Transmit offsetD 586D Transmit offsetD 587D StepD Transmit offsetD 588D Transmit offsetD 589D StepD Transmit offsetD 590D Transmit offsetD 591D StepD Transmit offsetD 592D Transmit offsetD 593D StepD Transmit offsetD 594D Transmit offsetD 595D Transmit offsetD 746D Transmit offsetD 747D Transmit offsetD 748D Transmit offsetD 749D Transmit offsetD 750D Transmit offsetD 751D StepD Transmit offsetD 596D Transmit offsetD 597D StepD Transmit offsetD 598D Transmit offsetD 599D Transmit offsetD 752D StepD Transmit offsetD 600D Transmit offsetD 601D StepD Transmit offsetD 602D Transmit offsetD 603D StepD Transmit offsetD 604D Transmit offsetD 605D StepD Transmit offsetD 606D Transmit offsetD 607D StepD Transmit offsetD 608D Transmit offsetD 609D StepD Transmit offsetD 610D Transmit offsetD 611D StepD Transmit offsetD 612D Transmit offsetD 613D StepD Transmit offsetD 616D Transmit offsetD 617D StepD Transmit offsetD 620D Transmit offsetD 621D StepD Transmit offsetD 624D Transmit offsetD 625D StepD Transmit offsetD 628D Transmit offsetD 629D Transmit offsetD 752D StepD Transmit offsetD 630D Transmit offsetD 631D StepD Transmit offsetD 554D Transmit offsetD 555D StepD Transmit offsetD 556D Transmit offsetD 557D Transmit offsetD 746D Transmit offsetD 747D Transmit offsetD 748D Transmit offsetD 749D Transmit offsetD 750D Transmit offsetD 751D StepD Transmit offsetD 558D Transmit offsetD 559D Transmit offsetD 746D Transmit offsetD 747D Transmit offsetD 748D Transmit offsetD 749D Transmit offsetD 750D Transmit offsetD 751D StepD Transmit offsetD 560D Transmit offsetD 561D StepD Transmit offsetD 562D Transmit offsetD 563D StepD Transmit offsetD 566D Transmit offsetD 567D StepD Transmit offsetD 568D Transmit offsetD 569D StepD Transmit offsetD 572D Transmit offsetD 573D StepD Transmit offsetD 574D Transmit offsetD 575D StepD Transmit offsetD 576D Transmit offsetD 577D Transmit offsetD 746D Transmit offsetD 747D Transmit offsetD 748D Transmit offsetD 749D Transmit offsetD 750D Transmit offsetD 751D StepD Transmit offsetD 578D Transmit offsetD 579D StepD Transmit offsetD 580D Transmit offsetD 581D StepD Transmit offsetD 582D Transmit offsetD 583D StepD Transmit offsetD 584D Transmit offsetD 585D StepD Transmit offsetD 588D Transmit offsetD 589D StepD Transmit offsetD 590D Transmit offsetD 591D StepD Transmit offsetD 592D Transmit offsetD 593D StepD Transmit offsetD 594D Transmit offsetD 595D Transmit offsetD 746D Transmit offsetD 747D Transmit offsetD 748D Transmit offsetD 749D Transmit offsetD 750D Transmit offsetD 751D StepD Transmit offsetD 596D Transmit offsetD 597D StepD Transmit offsetD 598D Transmit offsetD 599D Transmit offsetD 752D StepD Transmit offsetD 600D Transmit offsetD 601D StepD Transmit offsetD 602D Transmit offsetD 603D StepD Transmit offsetD 604D Transmit offsetD 605D StepD Transmit offsetD 606D Transmit offsetD 607D StepD Transmit offsetD 608D Transmit offsetD 609D StepD Transmit offsetD 610D Transmit offsetD 611D StepD Transmit offsetD 612D Transmit offsetD 613D StepD Transmit offsetD 616D Transmit offsetD 617D StepD Transmit offsetD 620D Transmit offsetD 621D StepD Transmit offsetD 624D Transmit offsetD 625D StepD Transmit offsetD 628D Transmit offsetD 629D Transmit offsetD 752D StepD Transmit offsetD 630D Transmit offsetD 631D StepD Transmit offsetD 554D Transmit offsetD 555D StepD Transmit offsetD 556D Transmit offsetD 557D Transmit offsetD 746D Transmit offsetD 747D Transmit offsetD 748D Transmit offsetD 749D Transmit offsetD 750D Transmit offsetD 751D StepD Transmit offsetD 558D Transmit offsetD 559D Transmit offsetD 746D Transmit offsetD 747D Transmit offsetD 748D Transmit offsetD 749D Transmit offsetD 750D Transmit offsetD 751D StepD Transmit offsetD 560D Transmit offsetD 561D StepD Transmit offsetD 562D Transmit offsetD 563D StepD Transmit offsetD 566D Transmit offsetD 567D StepD Transmit offsetD 568D Transmit offsetD 569D StepD Transmit offsetD 572D Transmit offsetD 573D StepD Transmit offsetD 574D Transmit offsetD 575D StepD Transmit offsetD 576D Transmit offsetD 577D Transmit offsetD 746D Transmit offsetD 747D Transmit offsetD 748D Transmit offsetD 749D Transmit offsetD 750D Transmit offsetD 751D StepD Transmit offsetD 578D Transmit offsetD 579D StepD Transmit offsetD 580D Transmit offsetD 581D StepD Transmit offsetD 582D Transmit offsetD 583D StepD Transmit offsetD 584D Transmit offsetD 585D StepD Transmit offsetD 588D Transmit offsetD 589D StepD Transmit offsetD 590D Transmit offsetD 591D StepD Transmit offsetD 592D Transmit offsetD 593D StepD Transmit offsetD 594D Transmit offsetD 595D Transmit offsetD 746D Transmit offsetD 747D Transmit offsetD 748D Transmit offsetD 749D Transmit offsetD 750D Transmit offsetD 751D StepD Transmit offsetD 596D Transmit offsetD 597D StepD Transmit offsetD 598D Transmit offsetD 599D Transmit offsetD 752D StepD Transmit offsetD 600D Transmit offsetD 601D StepD Transmit offsetD 602D Transmit offsetD 603D StepD Transmit offsetD 604D Transmit offsetD 605D StepD Transmit offsetD 606D Transmit offsetD 607D StepD Transmit offsetD 608D Transmit offsetD 609D StepD Transmit offsetD 610D Transmit offsetD 611D StepD Transmit offsetD 612D Transmit offsetD 613D StepD Transmit offsetD 616D Transmit offsetD 617D StepD Transmit offsetD 620D Transmit offsetD 621D StepD Transmit offsetD 624D Transmit offsetD 625D StepD Transmit offsetD 628D Transmit offsetD 629D Transmit offsetD 752D StepD Transmit offsetD 630D Transmit offsetD 631D StepD Transmit offsetD 554D Transmit offsetD 555D StepD Transmit offsetD 556D Transmit offsetD 557D Transmit offsetD 746D Transmit offsetD 747D Transmit offsetD 748D Transmit offsetD 749D Transmit offsetD 750D Transmit offsetD 751D StepD Transmit offsetD 558D Transmit offsetD 559D Transmit offsetD 746D Transmit offsetD 747D Transmit offsetD 748D Transmit offsetD 749D Transmit offsetD 750D Transmit offsetD 751D StepD Transmit offsetD 560D Transmit offsetD 561D StepD Transmit offsetD 562D Transmit offsetD 563D StepD Transmit offsetD 566D Transmit offsetD 567D StepD Transmit offsetD 568D Transmit offsetD 569D StepD Transmit offsetD 572D Transmit offsetD 573D StepD Transmit offsetD 574D Transmit offsetD 575D StepD Transmit offsetD 576D Transmit offsetD 577D Transmit offsetD 746D Transmit offsetD 747D Transmit offsetD 748D Transmit offsetD 749D Transmit offsetD 750D Transmit offsetD 751D StepD Transmit offsetD 578D Transmit offsetD 579D StepD Transmit offsetD 580D Transmit offsetD 581D StepD Transmit offsetD 582D Transmit offsetD 583D StepD Transmit offsetD 584D Transmit offsetD 585D StepD Transmit offsetD 588D Transmit offsetD 589D StepD Transmit offsetD 590D Transmit offsetD 591D StepD Transmit offsetD 592D Transmit offsetD 593D StepD Tra
    ((pc 10948)
     (general
      (0 6932 27280 0 0 1 11492 15 27296 10 594 100 594 0 0 148 373 0 28212 16607
       12227 8 594 594 3 10000 11628 12417 0 0 0 0)))
    ("00000000  93 00 00 00 13 01 00 00  93 01 00 00 13 02 00 00  |................|"
     "00000010  93 02 00 00 13 03 00 00  93 03 00 00 13 04 00 00  |................|"
     "00000020  93 04 00 00 13 05 00 00  93 05 00 00 13 06 00 00  |................|"
     "00000030  93 06 00 00 13 07 00 00  93 07 00 00 13 08 00 00  |................|"
     "00000040  93 08 00 00 13 09 00 00  93 09 00 00 13 0a 00 00  |................|"
     "00000050  93 0a 00 00 13 0b 00 00  93 0b 00 00 13 0c 00 00  |................|"
     "00000060  93 0c 00 00 13 0d 00 00  93 0d 00 00 13 0e 00 00  |................|"
     "00000070  93 0e 00 00 13 0f 00 00  93 0f 00 00 17 71 00 00  |.............q..|"
     "00000080  13 01 41 f8 ef 10 00 47  6f 00 00 00 93 02 05 00  |..A....Go.......|"
     "00000090  13 83 05 00 93 03 06 00  73 00 00 00 13 85 02 00  |........s.......|"
     "000000a0  67 80 00 00 13 01 01 fd  23 26 11 02 23 24 81 02  |g.......#&..#$..|"
     "000000b0  23 22 91 02 13 06 06 f2  93 06 f0 01 13 04 05 00  |#\"..............|"
     "000000c0  63 e0 c6 0e 13 16 26 00  37 35 00 00 13 05 c5 ce  |c.....&.75......|"
     "000000d0  33 05 a6 00 03 25 05 00  67 00 05 00 03 a5 45 00  |3....%..g.....E.|"
     "000000e0  13 06 10 00 13 16 b6 00  93 05 00 00 97 30 00 00  |.............0..|"
     "000000f0  e7 80 40 88 6f 00 c0 0c  b7 34 00 00 93 84 84 e6  |..@.o....4......|"
     "00000100  13 06 90 00 13 05 00 00  93 85 04 00 97 00 00 00  |................|"
     "00000110  e7 80 00 f8 e3 06 05 fe  83 25 84 12 13 85 e5 ff  |.........%......|"
     "00000120  13 06 f0 0f 23 24 a4 12  63 6c a6 0e 93 85 f5 ff  |....#$..cl......|"
     "00000130  13 06 00 10 63 f0 c5 10  13 06 84 02 33 05 a6 00  |....c.......3...|"
     "00000140  b3 05 b6 00 03 45 05 00  83 c5 05 00 13 15 85 00  |.....E..........|"
     "00000150  33 65 b5 00 6f 00 40 07  b7 34 00 00 93 84 44 e9  |3e..o.@..4....D.|"
     "00000160  13 06 e0 01 13 05 00 00  93 85 04 00 97 00 00 00  |................|"
     "00000170  e7 80 00 f2 e3 06 05 fe  6f 00 80 04 b7 34 00 00  |........o....4..|"
     "00000180  93 84 24 eb 13 06 60 01  13 05 00 00 93 85 04 00  |..$...`.........|"
     "00000190  97 00 00 00 e7 80 c0 ef  e3 06 05 fe 6f 00 40 02  |............o.@.|"
     "000001a0  b7 34 00 00 93 84 04 f0  13 06 90 01 13 05 00 00  |.4..............|"
     "000001b0  93 85 04 00 97 00 00 00  e7 80 80 ed e3 06 05 fe  |................|"
     "000001c0  03 15 04 13 13 05 25 00  23 18 a4 12 83 20 c1 02  |......%.#.... ..|"
     "000001d0  03 24 81 02 83 24 41 02  13 01 01 03 67 80 00 00  |.$...$A.....g...|"
     "000001e0  97 10 00 00 e7 80 c0 ed  37 35 00 00 13 05 85 ee  |........75......|"
     "000001f0  23 26 a1 00 13 05 10 00  23 28 a1 00 23 2e 01 00  |#&......#(..#...|"
     "00000200  13 05 40 00 23 2a a1 00  23 2c 01 00 b7 35 00 00  |..@.#*..#,...5..|"
     "00000210  93 85 05 ef 13 05 c1 00  97 20 00 00 e7 80 40 c2  |......... ....@.|"
     "00000220  37 36 00 00 13 06 46 e7  93 05 00 10 97 20 00 00  |76....F...... ..|"
     "00000230  e7 80 c0 c3 37 36 00 00  13 06 46 e8 13 05 00 10  |....76....F.....|"
     "00000240  93 05 00 10 97 20 00 00  e7 80 40 c2 23 18 c5 12  |..... ....@.#...|"
     "00000250  67 80 00 00 13 01 01 fe  23 2e 11 00 23 2c 81 00  |g.......#...#,..|"
     "00000260  23 2a 91 00 23 28 21 01  23 26 31 01 13 04 05 00  |#*..#(!.#&1.....|"
     "00000270  83 59 05 13 93 04 06 00  37 39 00 00 13 09 49 e3  |.Y......79....I.|"
     "00000280  13 06 a0 00 13 05 00 00  93 05 09 00 97 00 00 00  |................|"
     "00000290  e7 80 00 e0 e3 06 05 fe  03 25 84 12 93 05 f0 0f  |.........%......|"
     "000002a0  63 ee a5 04 93 89 29 00  13 d6 89 00 93 05 84 02  |c.....).........|"
     "000002b0  33 85 a5 00 23 00 c5 00  03 25 84 12 13 05 15 00  |3...#....%......|"
     "000002c0  13 06 00 10 63 76 c5 04  33 85 a5 00 23 00 35 01  |....cv..3...#.5.|"
     "000002d0  03 25 84 12 13 05 25 00  23 24 a4 12 23 18 94 12  |.%....%.#$..#...|"
     "000002e0  83 20 c1 01 03 24 81 01  83 24 41 01 03 29 01 01  |. ...$...$A..)..|"
     "000002f0  83 29 c1 00 13 01 01 02  67 80 00 00 37 36 00 00  |.)......g...76..|"
     "00000300  13 06 86 e4 93 05 00 10  97 20 00 00 e7 80 00 b6  |......... ......|"
     "00000310  37 36 00 00 13 06 86 e5  93 05 00 10 97 20 00 00  |76........... ..|"
     "00000320  e7 80 c0 b4 93 15 46 01  93 d5 c5 01 b3 05 b5 00  |......F.........|"
     "00000330  83 c6 85 01 13 76 f6 0f  93 05 40 00 63 84 c6 00  |.....v....@.c...|"
     "00000340  93 05 20 00 03 16 05 13  b3 85 c5 00 23 18 b5 12  |.. .........#...|"
     "00000350  67 80 00 00 93 15 46 01  93 d5 c5 01 b3 05 b5 00  |g.....F.........|"
     "00000360  83 c6 85 01 13 76 f6 0f  93 05 20 00 63 84 c6 00  |.....v.... .c...|"
     "00000370  93 05 40 00 03 16 05 13  b3 85 c5 00 23 18 b5 12  |..@.........#...|"
     "00000380  67 80 00 00 93 15 86 01  93 d5 c5 01 93 06 85 01  |g...............|"
     "00000390  13 16 46 01 13 56 c6 01  33 86 c6 00 03 46 06 00  |..F..V..3....F..|"
     "000003a0  b3 85 b6 00 83 c6 05 00  93 05 40 00 63 04 d6 00  |..........@.c...|"
     "000003b0  93 05 20 00 03 16 05 13  b3 85 c5 00 23 18 b5 12  |.. .........#...|"
     "000003c0  67 80 00 00 93 15 46 01  83 16 05 13 93 d5 c5 01  |g.....F.........|"
     "000003d0  b3 05 b5 00 23 8c c5 00  93 86 26 00 23 18 d5 12  |....#.....&.#...|"
     "000003e0  67 80 00 00 93 15 46 01  93 d5 c5 01 b3 05 b5 00  |g.....F.........|"
     "000003f0  83 c6 85 01 03 17 05 13  33 86 c6 00 23 8c c5 00  |........3...#...|"
     "00000400  13 07 27 00 23 18 e5 12  67 80 00 00 13 77 f6 00  |..'.#...g....w..|"
     "00000410  93 07 80 00 63 ea e7 00  13 17 27 00 33 87 e6 00  |....c.....'.3...|"
     "00000420  03 23 07 04 67 00 03 00  37 36 00 00 13 06 c6 f1  |.#..g...76......|"
     "00000430  93 05 90 00 13 05 07 00  97 20 00 00 e7 80 00 a3  |......... ......|"
     "00000440  93 15 86 01 93 d5 c5 01  93 06 85 01 13 16 46 01  |..............F.|"
     "00000450  13 56 c6 01 33 86 c6 00  03 46 06 00 b3 85 b6 00  |.V..3....F......|"
     "00000460  83 c6 05 00 93 05 20 00  63 04 d6 00 93 05 40 00  |...... .c.....@.|"
     "00000470  03 16 05 13 b3 85 c5 00  23 18 b5 12 67 80 00 00  |........#...g...|"
     "00000480  83 15 05 13 23 19 c5 12  93 85 25 00 23 18 b5 12  |....#.....%.#...|"
     "00000490  67 80 00 00 83 45 85 01  b3 85 c5 00 23 18 b5 12  |g....E......#...|"
     "000004a0  67 80 00 00 83 25 c5 12  93 96 d5 00 b3 c5 b6 00  |g....%..........|"
     "000004b0  93 d6 15 01 b3 c5 b6 00  93 96 55 00 b3 c5 b6 00  |..........U.....|"
     "000004c0  23 26 b5 12 93 16 46 01  b3 f5 c5 00 03 16 05 13  |#&....F.........|"
     "000004d0  93 d6 c6 01 b3 06 d5 00  23 8c b6 00 13 06 26 00  |........#.....&.|"
     "000004e0  23 18 c5 12 67 80 00 00  13 01 01 fc 23 2e 11 02  |#...g.......#...|"
     "000004f0  23 2c 81 02 23 2a 91 02  23 28 21 03 23 26 31 03  |#,..#*..#(!.#&1.|"
     "00000500  23 24 41 03 23 22 51 03  23 20 61 03 23 2e 71 01  |#$A.#\"Q.# a.#.q.|"
     "00000510  23 2c 81 01 23 2a 91 01  23 28 a1 01 23 26 b1 01  |#,..#*..#(..#&..|"
     "00000520  93 7a f6 00 23 20 a1 00  63 82 0a 22 93 09 00 00  |.z..# ..c..\"....|"
     "00000530  13 07 85 01 93 16 86 01  93 d6 c6 01 b3 06 d7 00  |................|"
     "00000540  03 c9 06 00 13 16 46 01  13 56 c6 01 33 06 c7 00  |......F..V..3...|"
     "00000550  03 46 06 00 83 54 25 13  03 a5 05 00 23 24 a1 00  |.F...T%.....#$..|"
     "00000560  03 aa 45 00 13 75 f6 03  23 22 a1 00 93 05 16 00  |..E..u..#\"......|"
     "00000570  13 fb f5 03 93 05 26 00  93 fb f5 03 93 05 36 00  |......&.......6.|"
     "00000580  13 fc f5 03 93 05 46 00  93 fc f5 03 93 05 56 00  |......F.......V.|"
     "00000590  13 fd f5 03 93 05 66 00  93 fd f5 03 13 05 76 00  |......f.......v.|"
     "000005a0  13 74 f5 03 13 19 69 00  b3 8a 54 01 6f 00 00 02  |.t....i...T.o...|"
     "000005b0  13 75 15 00 33 c5 a6 00  23 00 a6 00 93 84 14 00  |.u..3...#.......|"
     "000005c0  13 09 09 04 93 89 05 00  63 84 54 19 03 25 81 00  |........c.T..%..|"
     "000005d0  93 85 04 00 97 10 00 00  e7 80 40 86 13 16 85 01  |..........@.....|"
     "000005e0  93 75 09 7c 83 26 41 00  b3 e6 d5 00 b3 06 da 00  |.u.|.&A.........|"
     "000005f0  03 c7 06 00 93 57 86 41  93 a7 07 00 13 56 f6 01  |.....W.A.....V..|"
     "00000600  33 46 c7 00 13 07 f7 ff  13 37 17 00 23 80 c6 00  |3F.......7..#...|"
     "00000610  33 e6 65 01 33 06 ca 00  83 46 06 00 33 f7 e7 00  |3.e.3....F..3...|"
     "00000620  93 17 95 01 93 d7 f7 01  33 c8 f6 00 93 86 f6 ff  |........3.......|"
     "00000630  93 b6 16 00 b3 f6 d7 00  23 00 06 01 33 e6 75 01  |........#...3.u.|"
     "00000640  33 06 ca 00 83 47 06 00  b3 66 d7 00 13 17 a5 01  |3....G...f......|"
     "00000650  13 57 f7 01 33 c8 e7 00  93 87 f7 ff 93 b7 17 00  |.W..3...........|"
     "00000660  23 00 06 01 33 e6 85 01  33 06 ca 00 03 48 06 00  |#...3...3....H..|"
     "00000670  33 77 f7 00 93 17 b5 01  93 d7 f7 01 b3 48 f8 00  |3w...........H..|"
     "00000680  13 08 f8 ff 13 38 18 00  b3 f7 07 01 33 67 f7 00  |.....8......3g..|"
     "00000690  23 00 16 01 33 e6 95 01  33 06 ca 00 83 47 06 00  |#...3...3....G..|"
     "000006a0  b3 e6 e6 00 13 17 c5 01  13 57 f7 01 33 c8 e7 00  |.........W..3...|"
     "000006b0  93 87 f7 ff 93 b7 17 00  23 00 06 01 33 e6 a5 01  |........#...3...|"
     "000006c0  33 06 ca 00 03 48 06 00  33 77 f7 00 93 17 d5 01  |3....H..3w......|"
     "000006d0  93 d7 f7 01 b3 48 f8 00  13 08 f8 ff 13 38 18 00  |.....H.......8..|"
     "000006e0  b3 f7 07 01 33 67 f7 00  23 00 16 01 33 e6 b5 01  |....3g..#...3...|"
     "000006f0  33 06 ca 00 83 47 06 00  33 e7 e6 00 13 58 15 00  |3....G..3....X..|"
     "00000700  93 76 18 00 b3 c6 d7 00  23 00 d6 00 b3 e5 85 00  |.v......#.......|"
     "00000710  33 06 ba 00 83 46 06 00  93 87 f7 ff 93 b5 17 00  |3....F..........|"
     "00000720  b3 75 b8 00 93 87 f6 ff  93 b7 17 00 b3 77 f5 00  |.u...........w..|"
     "00000730  b3 e5 f5 00 b3 e5 e5 00  13 f7 15 00 93 05 10 00  |................|"
     "00000740  e3 18 07 e6 93 85 09 00  6f f0 9f e6 93 05 00 00  |........o.......|"
     "00000750  03 26 01 00 03 15 06 13  a3 03 b6 02 13 05 25 00  |.&............%.|"
     "00000760  23 18 a6 12 83 20 c1 03  03 24 81 03 83 24 41 03  |#.... ...$...$A.|"
     "00000770  03 29 01 03 83 29 c1 02  03 2a 81 02 83 2a 41 02  |.)...)...*...*A.|"
     "00000780  03 2b 01 02 83 2b c1 01  03 2c 81 01 83 2c 41 01  |.+...+...,...,A.|"
     "00000790  03 2d 01 01 83 2d c1 00  13 01 01 04 67 80 00 00  |.-...-......g...|"
     "000007a0  13 01 01 fd 93 15 46 01  93 d5 c5 01 b3 05 b5 00  |......F.........|"
     "000007b0  83 c6 85 01 93 05 f0 00  63 ea d5 04 b3 06 d5 00  |........c.......|"
     "000007c0  83 c5 86 00 13 76 f6 0f  93 06 e0 09 23 17 c1 00  |.....v......#...|"
     "000007d0  63 02 d6 02 93 06 10 0a  63 16 d6 04 03 56 05 13  |c.......c....V..|"
     "000007e0  63 9e 05 00 13 06 46 00  23 18 c5 12 13 01 01 03  |c.....F.#.......|"
     "000007f0  67 80 00 00 03 56 05 13  e3 96 05 fe 13 06 26 00  |g....V........&.|"
     ...
     "0000f800  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000f810  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000f820  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000f830  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000f840  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000f850  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000f860  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000f870  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000f880  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000f890  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000f8a0  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000f8b0  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000f8c0  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000f8d0  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000f8e0  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000f8f0  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000f900  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000f910  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000f920  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000f930  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000f940  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000f950  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000f960  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000f970  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000f980  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000f990  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000f9a0  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000f9b0  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000f9c0  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000f9d0  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000f9e0  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000f9f0  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000fa00  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000fa10  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000fa20  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000fa30  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000fa40  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000fa50  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000fa60  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000fa70  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000fa80  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000fa90  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000faa0  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000fab0  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000fac0  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000fad0  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000fae0  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000faf0  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000fb00  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000fb10  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000fb20  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000fb30  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000fb40  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000fb50  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000fb60  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000fb70  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000fb80  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000fb90  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000fba0  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000fbb0  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000fbc0  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000fbd0  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000fbe0  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000fbf0  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000fc00  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000fc10  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000fc20  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000fc30  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000fc40  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000fc50  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000fc60  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000fc70  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000fc80  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000fc90  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000fca0  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000fcb0  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000fcc0  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000fcd0  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000fce0  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000fcf0  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000fd00  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000fd10  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000fd20  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000fd30  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000fd40  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000fd50  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000fd60  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000fd70  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000fd80  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000fd90  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000fda0  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000fdb0  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000fdc0  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000fdd0  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000fde0  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000fdf0  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000fe00  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000fe10  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000fe20  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000fe30  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000fe40  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000fe50  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000fe60  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000fe70  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000fe80  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000fe90  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000fea0  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000feb0  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000fec0  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000fed0  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000fee0  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000fef0  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000ff00  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000ff10  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000ff20  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000ff30  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000ff40  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000ff50  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000ff60  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000ff70  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000ff80  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000ff90  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000ffa0  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000ffb0  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000ffc0  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000ffd0  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000ffe0  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "0000fff0  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|")
    |}]
;;
