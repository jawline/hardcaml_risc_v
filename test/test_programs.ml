open! Core
open Hardcaml
open Hardcaml_risc_v
open Hardcaml_risc_v_hart
open Hardcaml_uart_controller
open Hardcaml_waveterm
open Opcode_helper
open! Bits

let debug = true

let uart_config =
  { Hardcaml_uart_controller.Config.clock_frequency = 200
  ; baud_rate = 200
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
  Cpu.Make
    (struct
      let register_width = Register_width.B32
      let num_registers = 32
    end)
    (struct
      let num_bytes = 4096
    end)
    (struct
      let num_harts = 1
      let include_io_controller = Io_controller_config.Uart_controller uart_config
    end)

module With_transmitter = struct
  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; data_in_valid : 'a
      ; data_in : 'a [@bits 8]
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t =
      { data_out_valid : 'a
      ; data_out : 'a [@bits 8]
      ; registers : 'a Cpu_with_dma_memory.Registers.t list [@length 1]
      }
    [@@deriving sexp_of, hardcaml]
  end

  let create scope { I.clock; clear; data_in_valid; data_in } =
    let { Uart_tx.O.uart_tx; _ } =
      Uart_tx.hierarchical
        ~instance:"tx"
        scope
        { Uart_tx.I.clock; clear; data_in_valid; data_in }
    in
    let { Cpu_with_dma_memory.O.registers; uart_tx = cpu_uart_tx; _ } =
      Cpu_with_dma_memory.hierarchical
        ~instance:"cpu"
        scope
        { clock; clear; uart_rx = uart_tx }
    in
    let { Uart_rx.O.data_out_valid; data_out; _ } =
      Uart_rx.hierarchical
        ~instance:"rx"
        scope
        { Uart_rx.I.clock; clear; uart_rx = cpu_uart_tx }
    in
    { O.registers; data_out_valid; data_out }
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
  let ram = Cyclesim.lookup_mem sim "main_memory_bram" |> Option.value_exn in
  Array.map ~f:(fun mut -> Bits.Mutable.to_bits mut |> Bits.to_int) ram
  |> Array.iter ~f:(fun v -> printf "%02x " v);
  printf "\n"
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
   * byte once every ~10 cycles (this is dependent on the number of stop
   * bits and the parity bit. *)
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
      (* TODO: Tighter loop *)
      loop_for 20)
    whole_packet
;;

let test ~data sim =
  let sim, _, _ = sim in
  let inputs : _ With_transmitter.I.t = Cyclesim.inputs sim in
  (* Initialize the main memory to some known values for testing. *)
  let initial_ram = Cyclesim.lookup_mem sim "main_memory_bram" |> Option.value_exn in
  Array.iter
    ~f:(fun tgt -> Bits.Mutable.copy_bits ~src:(Bits.of_int ~width:8 0) ~dst:tgt)
    initial_ram;
  (* Send a clear signal to initialize any CPU IO controller state back to
   * default so we're ready to receive. *)
  clear_registers ~inputs sim;
  send_dma_message ~address:0 ~packet:data sim;
  let _outputs_before : _ With_transmitter.O.t =
    Cyclesim.outputs ~clock_edge:Side.Before sim
  in
  let outputs : _ With_transmitter.O.t = Cyclesim.outputs sim in
  clear_registers ~inputs sim;
  let rec loop_for cycles =
    if cycles = 0
    then ()
    else (
      Cyclesim.cycle sim;
      if Bits.to_bool !(outputs.data_out_valid)
      then printf "%c" (Bits.to_char !(outputs.data_out));
      loop_for (cycles - 1))
  in
  printf "RECEIVED FROM CPU VIA DMA: ";
  loop_for 20_000;
  printf "\n";
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
  test ~data:program sim;
  finalize_sim sim;
  [%expect
    {|
   RECEIVED FROM CPU VIA DMA: D Hello world!D Good
   ((pc 216)
    (general
     (0 216 2032 0 0 0 236 4 0 0 0 236 4 0 240 4 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
      0)))
   7ff00113 110113 b0000ef 6f ff010113 a12623 b12423 c12223 c12283 812303 412383 73 13 1010113 8067 fe010113 a12623 c12783 f12e23 100006f c12783 178793 f12623 c12783 7c783 fe0796e3 c12703 1c12783 40f707b3 78513 2010113 8067 fe010113 112e23 a12623 c12503 fadff0ef 50793 78613 c12583 513 f6dff0ef 13 1c12083 2010113 8067 ff010113 112623 f402783 78513 fb9ff0ef f802783 78513 fadff0ef 6f 6c6c6548 6f77206f 21646c72 00 646f6f47 657962 dc ec 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 f0 00 04 ec 00 00 00 00 ec 00 00 00 d8 00 00 00 0c 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 |}]
;;

let%expect_test "Game of life" =
  let program = In_channel.read_all "../test-programs/game_of_life/game_of_life" in
  let sim = create_sim "test_dma_game_of_life" in
  test ~data:program sim;
  finalize_sim sim;
  [%expect
    {|
    RECEIVED FROM CPU VIA DMA: D Starting up
    ((pc 632)
     (general
      (0 596 1920 0 0 0 1360 11 0 0 128 15 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0)))
    7ff00113 110113 4e4000ef 6f ff010113 a12623 b12423 c12223 c12283 812303 412383 73 13 1010113 8067 fe010113 112e23 a12623 b12423 812783 78613 c12583 513 fb5ff0ef 13 1c12083 2010113 8067 fe010113 a12623 b12423 60793 f103a3 12e23 240006f c12703 1c12783 f707b3 714703 e78023 1c12783 178793 f12e23 812783 1c12703 fcf76ce3 13 13 2010113 8067 fe010113 a12623 b12423 c12223 812703 5000793 e7f663 793 4c0006f 412703 1800793 e7f663 793 380006f 812783 37d793 f12e23 412703 70793 279793 e787b3 179793 78713 1c12783 f707b3 c12703 f707b3 78513 2010113 8067 ff010113 a12623 c12783 77f793 100713 f717b3 78513 1010113 8067 fd010113 2112623 2812423 a12623 b12423 c12223 412603 812583 c12503 f41ff0ef a12e23 1c12783 2078263 1c12783 7c783 78413 812503 f99ff0ef 50793 f477b3 80006f 793 78513 2c12083 2812403 3010113 8067 fd010113 2112623 2812423 a12623 b12423 c12223 d12023 412603 812583 c12503 ed1ff0ef a12e23 1c12783 6078e63 12783 2078863 1c12783 7c403 812503 f25ff0ef 50793 ff7f793 f467b3 ff7f713 1c12783 e78023 480006f 1c12783 7c783 1879413 41845413 812503 ef1ff0ef 50793 17b793 ff7f793 1879793 4187d793 f477b3 1879793 4187d793 ff7f713 1c12783 e78023 13 2c12083 2812403 3010113 8067 fd010113 2112623 a12623 b12423 c12223 12e23 412783 fff78793 f12c23 800006f 812703 412783 40f707b3 f12a23 500006f 1412783 812703 2f70c63 1812783 412703 2f70663 1412783 1812703 70613 78593 c12503 e69ff0ef 50713 1c12783 e787b3 f12e23 1812783 178793 f12c23 812783 178713 1412783 faf774e3 1812783 178793 f12c23 412783 178713 1812783 f6f77ce3 1c12783 78513 2c12083 3010113 8067 fd010113 2112623 a12623 b12423 12e23 7c0006f 12c23 5c0006f 1c12603 1812583 812503 f0dff0ef a12a23 1412703 100793 e7dc63 1412703 300793 e7c663 100793 80006f 793 78693 1c12603 1812583 c12503 e0dff0ef 1812783 178793 f12c23 1812703 4f00793 fae7f0e3 1c12783 178793 f12e23 1c12703 1700793 f8e7f0e3 13 13 2c12083 3010113 8067 fd010113 2112623 a12623 b12423 c12223 12e23 4c0006f 1c12783 412703 70613 78593 812503 d29ff0ef 50793 78663 2a00793 80006f 2000793 1c12703 c12683 e68733 f70023 1c12783 178793 f12e23 1c12703 4f00793 fae7d8e3 13 13 2c12083 3010113 8067 fd010113 2112623 a12623 12e23 2c0006f 1c12603 c12583 55c00513 f5dff0ef 5000593 55c00513 b81ff0ef 1c12783 178793 f12e23 1c12703 1700793 fce7d8e3 13 13 2c12083 3010113 8067 fe010113 112e23 b00593 55000513 b41ff0ef 64c00793 f12623 73c00793 f12423 613 f000593 812503 b55ff0ef c12583 812503 e35ff0ef 812503 f61ff0ef c12783 f12223 812783 f12623 412783 f12423 fc5ff06f 72617453 676e6974 707520 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 0f 73c 00 00 00 0f 00 00 0f 73c 00 00 ffffffff 73d 00 00 00 3c8 00 00 64c 73c 00 00 0f 00 00 00 00 52c 00 00 73c 64c 00 00 00 0c 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 |}]
;;
