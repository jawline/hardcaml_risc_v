open! Core
open Hardcaml
open Hardcaml_waveterm
open Hardcaml_uart
open Hardcaml_io_controller
open Hardcaml_memory_controller
open! Bits

let debug = true

module Memory_controller = Memory_controller.Make (struct
    let capacity_in_bytes = 128
    let num_read_channels = 1
    let num_write_channels = 1
    let address_width = 32
    let data_bus_width = 32
  end)

let print_ram sim =
  let ram =
    Cyclesim.lookup_mem_by_name sim "main_memory_bram"
    |> Option.value_exn
    |> Cyclesim.Memory.read_all
  in
  let as_str =
    Array.to_list ram
    |> List.map ~f:(fun t -> Bits.split_lsb ~part_width:8 t |> List.map ~f:Bits.to_char)
    |> List.concat
    |> String.of_char_list
  in
  print_s [%message "" ~_:(as_str : String.Hexdump.t)]
;;

let test ~name ~clock_frequency ~baud_rate ~include_parity_bit ~stop_bits ~address ~packet
  =
  let all_inputs =
    (* We add the header and then the packet length before the packet *)
    let packet = String.to_list packet in
    let packet_len_parts =
      Bits.of_int ~width:16 (List.length packet + 4)
      |> split_msb ~part_width:8
      |> List.map ~f:Bits.to_int
    in
    let address =
      Bits.of_int ~width:32 address |> split_msb ~part_width:8 |> List.map ~f:Bits.to_int
    in
    [ Char.to_int 'Q' ] @ packet_len_parts @ address @ List.map ~f:Char.to_int packet
  in
  let module Config = struct
    (* This should trigger a switch every other cycle. *)
    let config =
      { Hardcaml_uart.Config.clock_frequency; baud_rate; include_parity_bit; stop_bits }
    ;;
  end
  in
  let module Packet =
    Packet.Make (struct
      let data_bus_width = 8
    end)
  in
  let module Dma = Packet_to_memory.Make (Memory_controller.Memory_bus) (Packet) in
  let module Uart_tx = Uart_tx.Make (Config) in
  let module Uart_rx = Uart_rx.Make (Config) in
  let module Serial_buffer =
    Serial_buffer.Make (struct
      let serial_input_width = 8
    end)
  in
  let module Serial_to_packet =
    Serial_to_packet.Make
      (struct
        let header = 'Q'
        let serial_input_width = 8
      end)
      (Packet)
  in
  let module Machine = struct
    open Signal
    open Always
    open Memory_controller.Memory_bus

    module I = struct
      type 'a t =
        { clock : 'a
        ; clear : 'a
        ; data_in_valid : 'a
        ; data_in : 'a [@bits 8]
        }
      [@@deriving hardcaml ~rtlmangle:"$"]
    end

    module O = struct
      type 'a t =
        { parity_error : 'a
        ; write_response : 'a Write_response.With_valid.t
        ; read_response : 'a Read_response.With_valid.t
        }
      [@@deriving hardcaml ~rtlmangle:"$"]
    end

    let create (scope : Scope.t) { I.clock; clear; data_in_valid; data_in } =
      let { Uart_tx.O.uart_tx; _ } =
        Uart_tx.hierarchical
          ~instance:"tx"
          scope
          { Uart_tx.I.clock; clear; data_in_valid; data_in }
      in
      let { Uart_rx.O.data_out_valid = uart_rx_valid
          ; data_out = uart_rx_data
          ; parity_error
          }
        =
        Uart_rx.hierarchical
          ~instance:"rx"
          scope
          { Uart_rx.I.clock; clear; uart_rx = uart_tx }
      in
      let serial_to_packet_ready = wire 1 in
      let { Serial_buffer.O.out_valid = serial_to_buffer_valid
          ; out_data = serial_to_buffer_data
          }
        =
        Serial_buffer.hierarchical
          ~instance:"serial_buffer"
          ~capacity:8192
          scope
          { Serial_buffer.I.clock
          ; clear
          ; in_valid = uart_rx_valid
          ; in_data = uart_rx_data
          ; out_ready = serial_to_packet_ready
          }
      in
      let dma_ready = wire 1 in
      let { Serial_to_packet.O.out; ready = serial_to_packet_ready' } =
        Serial_to_packet.hierarchical
          ~instance:"serial_to_packet"
          scope
          { Serial_to_packet.I.clock
          ; clear
          ; in_valid = serial_to_buffer_valid
          ; in_data = serial_to_buffer_data
          ; out = { ready = vdd }
          }
      in
      serial_to_packet_ready <== serial_to_packet_ready';
      let dma_to_memory_controller = Write_bus.Rx.Of_always.wire zero in
      let memory_controller_to_dma = Write_response.With_valid.Of_always.wire zero in
      let dma =
        Dma.hierarchical
          ~instance:"dma"
          scope
          { Dma.I.clock
          ; clear
          ; in_ = out
          ; out = Write_bus.Rx.Of_always.value dma_to_memory_controller
          ; out_ack = Write_response.With_valid.Of_always.value memory_controller_to_dma
          }
      in
      dma_ready <== dma.in_.ready;
      let controller =
        Memory_controller.hierarchical
          ~instance:"memory_controller"
          ~priority_mode:Priority_order
          ~request_delay:1
          ~read_latency:1
          scope
          { Memory_controller.I.clock
          ; clear
          ; read_to_controller = [ Read_bus.Tx.Of_signal.of_int 0 ]
          ; write_to_controller = [ dma.out ]
          }
      in
      compile
        [ Write_bus.Rx.Of_always.assign
            dma_to_memory_controller
            (List.nth_exn controller.write_to_controller 0)
        ; Write_response.With_valid.Of_always.assign
            memory_controller_to_dma
            (List.nth_exn controller.write_response 0)
        ];
      (* We echo the read and write responses to avoid dead code elimination
         deleting the entire BRAM *)
      { O.parity_error
      ; write_response = List.nth_exn controller.write_response 0
      ; read_response = List.nth_exn controller.read_response 0
      }
    ;;
  end
  in
  let create_sim () =
    let module Sim = Cyclesim.With_interface (Machine.I) (Machine.O) in
    Sim.create
      ~config:Cyclesim.Config.trace_all
      (Machine.create
         (Scope.create ~auto_label_hierarchical_ports:true ~flatten_design:true ()))
  in
  let sim = create_sim () in
  let waveform, sim = Waveform.create sim in
  let inputs : _ Machine.I.t = Cyclesim.inputs sim in
  (* The fifo needs a clear cycle to initialize *)
  inputs.clear := vdd;
  Cyclesim.cycle sim;
  inputs.clear := gnd;
  Sequence.range 0 50 |> Sequence.iter ~f:(fun _ -> Cyclesim.cycle sim);
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
    all_inputs;
  loop_for 1000;
  if debug then Waveform.Serialize.marshall waveform name;
  print_ram sim
;;

let%expect_test "test" =
  test
    ~name:"/tmp/test_dma_hio"
    ~clock_frequency:200
    ~baud_rate:50
    ~include_parity_bit:false
    ~stop_bits:1
    ~address:0
    ~packet:"Hio";
  [%expect
    {|
    ("00000000  48 69 6f 00 00 00 00 00  00 00 00 00 00 00 00 00  |Hio.............|"
     "00000010  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "00000020  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "00000030  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "00000040  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "00000050  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "00000060  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "00000070  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|")
    |}];
  test
    ~name:"/tmp/test_dma_hello_world"
    ~clock_frequency:200
    ~baud_rate:50
    ~include_parity_bit:false
    ~stop_bits:1
    ~address:0
    ~packet:"Hello world!";
  [%expect
    {|
    ("00000000  48 65 6c 6c 6f 20 77 6f  72 6c 64 21 00 00 00 00  |Hello world!....|"
     "00000010  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "00000020  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "00000030  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "00000040  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "00000050  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "00000060  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "00000070  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|")
    |}]
;;
