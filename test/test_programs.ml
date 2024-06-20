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
  loop_for 10_000;
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
    RECEIVED FROM CPU VIA DMA:
    ((pc 0)
     (general
      (0 0 2032 0 0 0 0 0 0 0 0 0 5 0 0 2032 0 0 0 0 0 0 0 0 0 0 0 0 4096 0 0 0)))
    7c0 110113 a4000ef 6f ff010113 a12623 b12423 c12223 c12283 812303 412383 73 13 1010113 8067 fe010113 a12623 c12783 f12e23 100006f c12783 178793 f12623 c12783 7c783 fe0796e3 c12703 1c12783 40f707b3 78513 2010113 8067 fe010113 112e23 a12623 500613 c12583 513 f79ff0ef 13 1c12083 2010113 8067 ff010113 112623 1e37 d802783 78513 fc1ff0ef 6f 6c6c6548 6f77206f 21646c72 00 c8 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 |}]
;;

let%expect_test "Game of life" =
  let program = In_channel.read_all "../test-programs/game_of_life/game_of_life" in
  let sim = create_sim "test_dma_game_of_life" in
  test ~data:program sim;
  finalize_sim sim;
  [%expect
    {|
    RECEIVED FROM CPU VIA DMA:
    ((pc 0)
     (general
      (0 0 2016 0 0 0 0 0 0 0 2016 2016 0 0 2016 23 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0)))
    7e0 110113 55c000ef 6f fe010113 812e23 2010413 fea42623 feb42423 fec42223 73 13 1c12403 2010113 8067 fe010113 112e23 812c23 2010413 fea42623 feb42423 fe842783 78613 fec42583 513 fadff0ef 13 1c12083 1812403 2010113 8067 fd010113 2812623 3010413 fca42e23 fcb42c23 60793 fcf40ba3 fe042623 240006f fdc42703 fec42783 f707b3 fd744703 e78023 fec42783 178793 fef42623 fd842783 fec42703 fcf76ce3 13 13 2c12403 3010113 8067 fd010113 2812623 3010413 fca42e23 fcb42c23 fcc42a23 fd842703 5000793 e7f663 793 4c0006f fd442703 1800793 e7f663 793 380006f fd842783 37d793 fef42623 fd442703 70793 279793 e787b3 179793 78713 fec42783 f707b3 fdc42703 f707b3 78513 2c12403 3010113 8067 fe010113 812e23 2010413 fea42623 fec42783 77f793 100713 f717b3 78513 1c12403 2010113 8067 fd010113 2112623 2812423 2912223 3010413 fca42e23 fcb42c23 fcc42a23 fd442603 fd842583 fdc42503 f21ff0ef fea42623 fec42783 2078263 fec42783 7c783 78493 fd842503 f85ff0ef 50793 f4f7b3 80006f 793 78513 2c12083 2812403 2412483 3010113 8067 fd010113 2112623 2812423 2912223 3010413 fca42e23 fcb42c23 fcc42a23 fcd42823 fd442603 fd842583 fdc42503 ea5ff0ef fea42623 fec42783 6078e63 fd042783 2078863 fec42783 7c483 fd842503 f05ff0ef 50793 ff7f793 f4e7b3 ff7f713 fec42783 e78023 480006f fec42783 7c783 1879493 4184d493 fd842503 ed1ff0ef 50793 17b793 ff7f793 1879793 4187d793 f4f7b3 1879793 4187d793 ff7f713 fec42783 e78023 13 2c12083 2812403 2412483 3010113 8067 fd010113 2112623 2812423 3010413 fca42e23 fcb42c23 fcc42a23 fe042623 fd442783 fff78793 fef42423 800006f fd842703 fd442783 40f707b3 fef42223 500006f fe442783 fd842703 2f70c63 fe842783 fd442703 2f70663 fe442783 fe842703 70613 78593 fdc42503 e49ff0ef 50713 fec42783 e787b3 fef42623 fe842783 178793 fef42423 fd842783 178713 fe442783 faf774e3 fe842783 178793 fef42423 fd442783 178713 fe842783 f6f77ce3 fec42783 78513 2c12083 2812403 3010113 8067 fd010113 2112623 2812423 3010413 fca42e23 fcb42c23 fe042623 7c0006f fe042423 5c0006f fec42603 fe842583 fd842503 ef9ff0ef fea42223 fe442703 100793 e7dc63 fe442703 300793 e7c663 100793 80006f 793 78693 fec42603 fe842583 fdc42503 dedff0ef fe842783 178793 fef42423 fe842703 4f00793 fae7f0e3 fec42783 178793 fef42623 fec42703 1700793 f8e7f0e3 13 13 2c12083 2812403 3010113 8067 fd010113 2112623 2812423 3010413 fca42e23 fcb42c23 fcc42a23 fe042623 4c0006f fec42783 fd442703 70613 78593 fd842503 cf1ff0ef 50793 78663 2a00793 80006f 2000793 fec42703 fdc42683 e68733 f70023 fec42783 178793 fef42623 fec42703 4f00793 fae7d8e3 13 13 2c12083 2812403 3010113 8067 fd010113 2112623 2812423 3010413 fca42e23 fe042623 2c0006f fec42603 fdc42583 5c400513 f49ff0ef 5000593 5c400513 b0dff0ef fec42783 178793 fef42623 fec42703 1700793 fce7d8e3 13 13 2c12083 2812403 3010113 8067 fe010113 112e23 812c23 2010413 6b400793 fef42623 7a400793 fef42423 613 f000593 fe842503 aedff0ef fec42583 fe842503 e15ff0ef fe842503 f59ff0ef fec42783 fef42223 fe842783 fef42623 fe442783 fef42423 fc5ff06f 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 7e0 00 00 00 00 00 00 00 800 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 |}]
;;
