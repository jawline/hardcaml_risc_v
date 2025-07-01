open! Core
open Hardcaml
open Hardcaml_test_harness
open Hardcaml_uart
open Hardcaml_io_framework
open Hardcaml_io_controller
open Hardcaml_memory_controller
open! Bits

let debug = false

module Memory_controller = Bram_memory_controller.Make (struct
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
    |> List.map ~f:(fun t -> split_lsb ~part_width:8 t |> List.map ~f:to_char)
    |> List.concat
    |> String.of_char_list
  in
  print_s [%message "" ~_:(as_str : String.Hexdump.t)]
;;

let test ~clock_frequency ~baud_rate ~include_parity_bit ~stop_bits ~address ~packet =
  let all_inputs =
    (* We add the header and then the packet length before the packet *)
    let packet = String.to_list packet in
    let packet_len_parts =
      of_unsigned_int ~width:16 (List.length packet + 4)
      |> split_msb ~part_width:8
      |> List.map ~f:to_int_trunc
    in
    let address =
      of_unsigned_int ~width:32 address
      |> split_msb ~part_width:8
      |> List.map ~f:to_int_trunc
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
  let module Dma = Packet_to_memory.Make (Memory_controller.Memory_bus) (Axi8) in
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
      end)
      (Axi8)
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
        ; ready_for_next_input : 'a
        ; write_response : 'a Write_response.With_valid.t
        ; read_response : 'a Read_response.With_valid.t
        }
      [@@deriving hardcaml ~rtlmangle:"$"]
    end

    let create (scope : Scope.t) { I.clock; clear; data_in_valid; data_in } =
      let { Uart_tx.O.uart_tx; idle = ready_for_next_input; _ } =
        Uart_tx.hierarchical scope { Uart_tx.I.clock; clear; data_in_valid; data_in }
      in
      let { Uart_rx.O.data_out_valid = uart_rx_valid
          ; data_out = uart_rx_data
          ; parity_error
          }
        =
        Uart_rx.hierarchical scope { Uart_rx.I.clock; clear; uart_rx = uart_tx }
      in
      let serial_to_packet_ready = wire 1 in
      let { Serial_buffer.O.out_valid = serial_to_buffer_valid
          ; out_data = serial_to_buffer_data
          }
        =
        Serial_buffer.hierarchical
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
      let { Serial_to_packet.O.dn; up_ready = serial_to_packet_ready' } =
        Serial_to_packet.hierarchical
          scope
          { Serial_to_packet.I.clock
          ; clear
          ; in_valid = serial_to_buffer_valid
          ; in_data = serial_to_buffer_data
          ; dn = { tready = vdd }
          }
      in
      Signal.(serial_to_packet_ready <-- serial_to_packet_ready');
      let dma_to_memory_controller = Write_bus.Dest.Of_always.wire zero in
      let memory_controller_to_dma = Write_response.With_valid.Of_always.wire zero in
      let dma =
        Dma.hierarchical
          scope
          { Dma.I.clock
          ; clear
          ; in_ = dn
          ; out = Write_bus.Dest.Of_always.value dma_to_memory_controller
          ; out_ack = Write_response.With_valid.Of_always.value memory_controller_to_dma
          }
      in
      Signal.(dma_ready <-- dma.in_.tready);
      let controller =
        Memory_controller.hierarchical
          ~build_mode:Simulation
          ~priority_mode:Priority_order
          ~read_latency:1
          scope
          { Memory_controller.I.clock
          ; clear
          ; read_to_controller = [ Read_bus.Source.Of_signal.zero () ]
          ; write_to_controller = [ dma.out ]
          }
      in
      compile
        [ Write_bus.Dest.Of_always.assign
            dma_to_memory_controller
            (List.nth_exn controller.write_to_controller 0)
        ; Write_response.With_valid.Of_always.assign
            memory_controller_to_dma
            (List.nth_exn controller.write_response 0)
        ];
      (* We echo the read and write responses to avoid dead code elimination
         deleting the entire BRAM *)
      { O.parity_error
      ; ready_for_next_input
      ; write_response = List.nth_exn controller.write_response 0
      ; read_response = List.nth_exn controller.read_response 0
      }
    ;;
  end
  in
  let module Harness = Cyclesim_harness.Make (Machine.I) (Machine.O) in
  Harness.run
    ~waves_config:(if debug then Waves_config.to_home_subdirectory () else No_waves)
    ~trace:`All_named
    ~create:Machine.create
    (fun ~inputs ~outputs sim ->
       (* The fifo needs a clear cycle to initialize *)
       inputs.clear := vdd;
       Cyclesim.cycle sim;
       inputs.clear := gnd;
       Sequence.range 0 50 |> Sequence.iter ~f:(fun _ -> Cyclesim.cycle sim);
       let rec loop_until_ready_for_next_input () =
         Cyclesim.cycle sim;
         if Bits.to_bool !(outputs.ready_for_next_input)
         then ()
         else loop_until_ready_for_next_input ()
       in
       List.iter
         ~f:(fun input ->
           inputs.data_in_valid := vdd;
           inputs.data_in := of_unsigned_int ~width:8 input;
           Cyclesim.cycle sim;
           inputs.data_in_valid := gnd;
           loop_until_ready_for_next_input ())
         all_inputs;
       (* Wait some cycles for the writes to all finalize. *)
       Cyclesim.cycle ~n:100 sim;
       print_ram sim)
;;

let%expect_test "test" =
  test
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
