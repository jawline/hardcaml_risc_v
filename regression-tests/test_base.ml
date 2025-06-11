open! Core
open Hardcaml
open Hardcaml_risc_v
open Hardcaml_risc_v_hart
open Hardcaml_uart
open Hardcaml_risc_v_test
open Hardcaml_waveterm
open Opcode_helper
open! Bits

let debug = false
let output_width = 64
let output_height = 35

let uart_config =
  { Hardcaml_uart.Config.clock_frequency = 200
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
      let design_frequency = 2000
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
              let output_width = output_width
              let output_height = output_width
              let input_width = 32
              let input_height = 32
              let framebuffer_address = 0x8000
            end : Video_out_intf.Config)
          , (module struct
              (* TODO: Add a clock requirement *)

              let h_active = output_width
              let v_active = output_height
              let h_fp = 1
              let h_sync = 1
              let h_bp = 1
              let v_fp = 1
              let v_sync = 8
              let v_bp = 1
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
      ; video_out : 'a Cpu_with_dma_memory.Video_out_with_memory.O.t
      ; registers : 'a Cpu_with_dma_memory.Registers.t list [@length 1]
      ; ready_for_next_input : 'a
      }
    [@@deriving sexp_of, hardcaml ~rtlmangle:"$"]
  end

  let create scope { I.clock; clear; data_in_valid; data_in } =
    let { Uart_tx.O.uart_tx; idle = ready_for_next_input; _ } =
      Uart_tx.hierarchical scope { Uart_tx.I.clock; clear; data_in_valid; data_in }
    in
    let { Cpu_with_dma_memory.O.registers; uart_tx = cpu_uart_tx; video_out; _ } =
      Cpu_with_dma_memory.hierarchical scope { clock; clear; uart_rx = Some uart_tx }
    in
    let { Uart_rx.O.data_out_valid; data_out; _ } =
      Uart_rx.hierarchical
        scope
        { Uart_rx.I.clock; clear; uart_rx = Option.value_exn cpu_uart_tx }
    in
    { O.registers
    ; data_out_valid
    ; data_out
    ; video_out = Option.value_exn video_out
    ; ready_for_next_input
    }
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
    |> concat_lsb
    |> split_lsb ~part_width:8
    |> List.map ~f:to_char
    |> String.of_char_list
  in
  print_s [%message "" ~_:(data : String.Hexdump.t)]
;;

let clear_registers ~(inputs : Bits.t ref With_transmitter.I.t) sim =
  inputs.clear := one 1;
  Cyclesim.cycle sim;
  inputs.clear := zero 1
;;

let send_dma_message ~address ~packet sim =
  let inputs : _ With_transmitter.I.t = Cyclesim.inputs sim in
  let outputs : _ With_transmitter.O.t = Cyclesim.outputs sim in
  let whole_packet = dma_packet ~address packet in
  (* Send the DMA message through byte by byte. Uart_tx will transmit a
     byte once every ~10 cycles (this is dependent on the number of stop
     bits and the parity bit. *)
  let rec loop_until_ready_for_next_input () =
    Cyclesim.cycle sim;
    if Bits.to_bool !(outputs.ready_for_next_input)
    then ()
    else loop_until_ready_for_next_input ()
  in
  List.iter
    ~f:(fun input ->
      inputs.data_in_valid := vdd;
      inputs.data_in := of_int_trunc ~width:8 input;
      Cyclesim.cycle sim;
      inputs.data_in_valid := gnd;
      loop_until_ready_for_next_input ())
    whole_packet
;;

let test ~print_frames ~cycles ~data sim =
  let sim, _, _ = sim in
  let inputs : _ With_transmitter.I.t = Cyclesim.inputs sim in
  (* Initialize the main memory to some known values for testing. *)
  let initial_ram =
    Cyclesim.lookup_mem_by_name sim "main_memory_bram" |> Option.value_exn
  in
  Sequence.range 0 (Cyclesim.Memory.size_in_words initial_ram)
  |> Sequence.iter ~f:(fun i -> Cyclesim.Memory.of_bits ~address:i initial_ram (zero 32));
  (* Send a clear signal to initialize any CPU IO controller state back to
   * default so we're ready to receive. *)
  clear_registers ~inputs sim;
  let video_emulator = Video_emulator.create ~width:output_width ~height:output_height in
  send_dma_message ~address:0 ~packet:data sim;
  let _outputs_before : _ With_transmitter.O.t =
    Cyclesim.outputs ~clock_edge:Side.Before sim
  in
  let outputs : _ With_transmitter.O.t = Cyclesim.outputs sim in
  (* Wait some arbitrary number of cycles for the actual DMA to proceed. This is hard to guess, since the memory controller can push back. *)
  Sequence.range 0 100 |> Sequence.iter ~f:(fun _ -> Cyclesim.cycle sim);
  (* Send a clear signal and then start the vsync logic *)
  clear_registers ~inputs sim;
  print_s [%message "Printing RAM before registers"];
  let rec loop_for cycles =
    if cycles = 0
    then ()
    else (
      Cyclesim.cycle sim;
      Video_emulator.cycle
        video_emulator
        ~video_data:!(outputs.video_out.video_data.vdata)
        ~video_signals:
          (Video_signals.Video_signals.map
             ~f:(fun t -> !t)
             outputs.video_out.video_signals);
      if to_bool !(outputs.data_out_valid) then printf "%c" (to_char !(outputs.data_out));
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
            let px = Array.get frame ((y * output_width) + x) <> 0 in
            printf "%s" (if px then "*" else "-"));
          printf "\n"))
      (* Frames are in reverse order because we're prepending. *)
      (List.rev video_emulator.frames);
  match outputs.registers with
  | [ outputs ] ->
    let outputs =
      Cpu_with_dma_memory.Registers.map ~f:(fun t -> to_int_trunc !t) outputs
    in
    print_s [%message "" ~_:(outputs : int Cpu_with_dma_memory.Registers.t)];
    print_ram sim
  | _ -> raise_s [%message "BUG: Unexpected number of harts"]
;;
