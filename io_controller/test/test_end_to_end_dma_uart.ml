open! Core
open Hardcaml
open Hardcaml_test_harness
open Hardcaml_uart
open Hardcaml_io_framework
open Hardcaml_io_controller
open Hardcaml_memory_controller
open! Bits

let debug = true

module Memory_controller = Memory_controller.Make (struct
    let capacity_in_bytes = 128
    let num_write_channels = 1
    let num_read_channels = 1
    let address_width = 32
    let data_bus_width = 32
  end)

open Memory_controller.Memory_bus

let test
      ~verbose
      ~clock_frequency
      ~baud_rate
      ~include_parity_bit
      ~stop_bits
      ~address
      ~packet
  =
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
  let module Serial_to_packet =
    Serial_to_packet.Make
      (struct
        let header = 'Q'
      end)
      (Axi8)
  in
  let module Memory_to_packet8 =
    Memory_to_packet8.Make
      (struct
        let header = Some 'Q'
      end)
      (Memory_controller.Memory_bus)
      (Axi8)
  in
  let module Machine = struct
    open Signal
    open Always

    module I = struct
      type 'a t =
        { clock : 'a
        ; clear : 'a
        ; data_in_valid : 'a
        ; data_in : 'a [@bits 8]
        ; dma_out_enable : 'a
        ; dma_out_address : 'a [@bits 32]
        ; dma_out_length : 'a [@bits 16]
        }
      [@@deriving hardcaml]
    end

    module O = struct
      type 'a t =
        { out_valid : 'a [@bits 1]
        ; out_data : 'a [@bits 8]
        }
      [@@deriving hardcaml]
    end

    let create
          (scope : Scope.t)
          { I.clock
          ; clear
          ; data_in_valid
          ; data_in
          ; dma_out_enable
          ; dma_out_address
          ; dma_out_length
          }
      =
      let { Uart_tx.O.uart_tx; _ } =
        Uart_tx.hierarchical
          ~instance:"tx"
          scope
          { Uart_tx.I.clock; clear; data_in_valid; data_in }
      in
      let { Uart_rx.O.data_out_valid; data_out; parity_error = _ } =
        Uart_rx.hierarchical
          ~instance:"rx"
          scope
          { Uart_rx.I.clock; clear; uart_rx = uart_tx }
      in
      let { Serial_to_packet.O.dn; up_ready = _ } =
        Serial_to_packet.hierarchical
          ~instance:"serial_to_packet"
          scope
          { Serial_to_packet.I.clock
          ; clear
          ; in_valid = data_out_valid
          ; in_data = data_out
          ; dn = { tready = vdd }
          }
      in
      let dma_to_memory_controller = Write_bus.Dest.Of_always.wire zero in
      let memory_controller_to_dma = Write_response.With_valid.Of_always.wire zero in
      let dma =
        Dma.hierarchical
          ~instance:"dma"
          scope
          { Dma.I.clock
          ; clear
          ; in_ = dn
          ; out = Write_bus.Dest.Of_always.value dma_to_memory_controller
          ; out_ack = Write_response.With_valid.Of_always.value memory_controller_to_dma
          }
      in
      let dma_out_to_memory_controller = Read_bus.Dest.Of_always.wire zero in
      let memory_controller_to_dma_out = Read_response.With_valid.Of_always.wire zero in
      let uart_tx_ready = wire 1 in
      let dma_out =
        Memory_to_packet8.hierarchical
          ~instance:"dma_out"
          scope
          { Memory_to_packet8.I.clock
          ; clear
          ; enable =
              { valid = dma_out_enable
              ; value = { address = dma_out_address; length = dma_out_length }
              }
          ; memory = Read_bus.Dest.Of_always.value dma_out_to_memory_controller
          ; memory_response =
              Read_response.With_valid.Of_always.value memory_controller_to_dma_out
          ; output_packet = { tready = uart_tx_ready }
          }
      in
      let dma_out_uart_tx =
        Uart_tx.hierarchical
          ~instance:"tx"
          scope
          { Uart_tx.I.clock
          ; clear
          ; data_in_valid = dma_out.output_packet.tvalid
          ; data_in = dma_out.output_packet.tdata
          }
      in
      Signal.(uart_tx_ready <-- dma_out_uart_tx.idle);
      let dma_out_uart_rx =
        Uart_rx.hierarchical
          ~instance:"tx_rx"
          scope
          { Uart_rx.I.clock; clear; uart_rx = dma_out_uart_tx.uart_tx }
      in
      let controller =
        Memory_controller.hierarchical
          ~instance:"memory_controller"
          ~priority_mode:Priority_order
          ~request_delay:1
          ~read_latency:1
          scope
          { Memory_controller.I.clock
          ; clear
          ; write_to_controller = [ dma.out ]
          ; read_to_controller = [ dma_out.memory ]
          }
      in
      compile
        [ Write_bus.Dest.Of_always.assign
            dma_to_memory_controller
            (List.nth_exn controller.write_to_controller 0)
        ; Write_response.With_valid.Of_always.assign
            memory_controller_to_dma
            (List.nth_exn controller.write_response 0)
        ; Read_bus.Dest.Of_always.assign
            dma_out_to_memory_controller
            (List.nth_exn controller.read_to_controller 0)
        ; Read_response.With_valid.Of_always.assign
            memory_controller_to_dma_out
            (List.nth_exn controller.read_response 0)
        ];
      { O.out_valid = dma_out_uart_rx.data_out_valid
      ; out_data = dma_out_uart_rx.data_out
      }
    ;;
  end
  in
  let module Harness = Cyclesim_harness.Make (Machine.I) (Machine.O) in
  Harness.run
    ~create:Machine.create
    ~trace:`All_named
    ~waves_config:Waves_config.No_waves
    (fun ~inputs ~outputs sim ->
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
           inputs.data_in := of_unsigned_int ~width:8 input;
           Cyclesim.cycle sim;
           inputs.data_in_valid := gnd;
           loop_for 50)
         all_inputs;
       loop_for 100;
       if verbose
       then (
         printf "Printing ram (not DMA response):\n";
         Test_util.print_ram sim;
         printf "Doing a DMA read:\n");
       let issue_read ~address ~length =
         inputs.dma_out_enable := vdd;
         inputs.dma_out_address := of_unsigned_int ~width:32 address;
         inputs.dma_out_length := of_unsigned_int ~width:16 length;
         Cyclesim.cycle sim;
         inputs.dma_out_enable := gnd;
         let data = ref "" in
         let store_outputs () =
           if to_bool !(outputs.out_valid)
           then
             data
             := String.concat [ !data; to_char !(outputs.out_data) |> Char.to_string ]
           else ()
         in
         let count = ref 0 in
         while !count <> 1500 do
           Cyclesim.cycle sim;
           store_outputs ();
           incr count
         done;
         let data = !data in
         let without_length = String.subo ~pos:3 ~len:(String.length data - 3) data in
         (* TODO: assert without length is the length of the two words. *)
         if String.(without_length <> packet)
         then raise_s [%message "BUG: Packet differs" ~received:without_length ~packet];
         if verbose
         then (
           print_s [%message "" ~_:(data : String.Hexdump.t)];
           printf "%i\n" !count)
         else ()
       in
       issue_read ~address ~length:(String.length packet))
;;

let%expect_test "test" =
  test
    ~clock_frequency:200
    ~baud_rate:50
    ~include_parity_bit:false
    ~stop_bits:1
    ~address:0
    ~packet:"Hio"
    ~verbose:true;
    (* The additional o is a side effect of the DMA modules inability to not write dw aligned. *)
    (* TODO: Use byte enables to only write the bytes the packet to memory module has buffered. *)
  [%expect
    {|
    Printing ram (not DMA response):
    ("00000000  48 69 6f 6f 00 00 00 00  00 00 00 00 00 00 00 00  |Hioo............|"
     "00000010  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "00000020  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "00000030  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "00000040  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "00000050  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "00000060  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "00000070  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|")
    Doing a DMA read:
    ("00000000  51 00 03 48 69 6f                                 |Q..Hio|")
    1500
    |}];
  test
    ~clock_frequency:200
    ~baud_rate:50
    ~include_parity_bit:false
    ~stop_bits:1
    ~address:0
    ~packet:"Hello world!"
    ~verbose:true;
  [%expect
    {|
    Printing ram (not DMA response):
    ("00000000  48 65 6c 6c 6f 20 77 6f  72 6c 64 21 00 00 00 00  |Hello world!....|"
     "00000010  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "00000020  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "00000030  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "00000040  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "00000050  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "00000060  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "00000070  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|")
    Doing a DMA read:
    ("00000000  51 00 0c 48 65 6c 6c 6f  20 77 6f 72 6c 64 21     |Q..Hello world!|")
    1500
    |}]
;;

let%expect_test "fuzz" =
  Quickcheck.test ~trials:50 String.gen_nonempty ~f:(fun test_str ->
    test
      ~clock_frequency:200
      ~baud_rate:50
      ~include_parity_bit:false
      ~stop_bits:1
      ~address:0
      ~packet:test_str
      ~verbose:false);
  [%expect
    {| |}]
;;
