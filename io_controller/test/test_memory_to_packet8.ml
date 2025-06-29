open! Core
open Hardcaml
open Hardcaml_test_harness
open Hardcaml_io_controller
open Hardcaml_memory_controller
open! Bits

let debug = false

module Packet = struct
  type t =
    { contents : String.t
    ; dma_address : int
    ; dma_length : int
    }
end

let test ~packets =
  let module Memory_controller =
    Bram_memory_controller.Make (struct
      let capacity_in_bytes = 256
      let num_write_channels = 1
      let num_read_channels = 1
      let address_width = 32
      let data_bus_width = 32
    end)
  in
  let module Memory_to_packet8 =
    Memory_to_packet8.Make
      (struct
        let header = Some 'Z'
      end)
      (Memory_controller.Memory_bus)
      (Axi8)
  in
  let module Machine = struct
    open Memory_controller.Memory_bus

    module I = struct
      type 'a t =
        { clock : 'a
        ; clear : 'a
        ; enable : 'a
        ; address : 'a [@bits 32]
        ; length : 'a [@bits 16]
        }
      [@@deriving hardcaml]
    end

    module O = Memory_to_packet8.O

    let create (scope : Scope.t) { I.clock; clear; enable; address; length } =
      let ch_to_controller = Read_bus.Source.Of_always.wire Signal.zero in
      let controller =
        Memory_controller.hierarchical
          ~build_mode:Simulation
          ~request_delay:1
          ~read_latency:1
          ~priority_mode:Priority_order
          scope
          { Memory_controller.I.clock
          ; clear
          ; read_to_controller = [ Read_bus.Source.Of_always.value ch_to_controller ]
          ; write_to_controller = [ Write_bus.Source.Of_signal.zero () ]
          }
      in
      let output =
        Memory_to_packet8.hierarchical
          scope
          { Memory_to_packet8.I.clock
          ; clear
          ; enable = { valid = enable; value = { address; length } }
          ; output_packet = { tready = Signal.vdd }
          ; memory = List.nth_exn controller.read_to_controller 0
          ; memory_response = List.nth_exn controller.read_response 0
          }
      in
      Always.compile [ Read_bus.Source.Of_always.assign ch_to_controller output.memory ];
      output
    ;;
  end
  in
  let module Harness = Cyclesim_harness.Make (Machine.I) (Machine.O) in
  Harness.run
    ~create:Machine.create
    ~trace:`All_named
    ~waves_config:(if debug then Waves_config.to_home_subdirectory () else No_waves)
    (fun ~inputs ~outputs sim ->
       List.iter
         ~f:(fun { Packet.contents = load_memory; dma_address; dma_length } ->
           Test_util.write_packet_to_memory ~packet:load_memory sim;
           let data = ref "" in
           let store_outputs () =
             if to_bool !(outputs.output_packet.tvalid)
             then
               data
               := String.concat
                    [ !data; to_char !(outputs.output_packet.tdata) |> Char.to_string ]
             else ()
           in
           let issue_read ~address ~length =
             inputs.enable := vdd;
             inputs.address := of_unsigned_int ~width:32 address;
             inputs.length := of_unsigned_int ~width:16 length;
             Cyclesim.cycle sim;
             store_outputs ();
             inputs.enable := gnd;
             let count = ref 0 in
             while !count <> 1000 && not (to_bool !(outputs.output_packet.tlast)) do
               Cyclesim.cycle sim;
               store_outputs ();
               incr count
             done;
             print_s [%message "" ~_:(!data : String.Hexdump.t)];
             printf "Cycles: %i\n" !count
           in
           inputs.clear := vdd;
           Cyclesim.cycle sim;
           inputs.clear := gnd;
           issue_read ~address:dma_address ~length:dma_length)
         packets)
;;

let%expect_test "test" =
  let test_str = "The quick brown fox jumps over the lazy dog" in
  test
    ~packets:
      [ { Packet.contents = test_str; dma_address = 3; dma_length = 9 }
      ; { Packet.contents = test_str
        ; dma_address = 0
        ; dma_length = String.length test_str
        }
      ; { Packet.contents = test_str; dma_address = 11; dma_length = 1 }
      ; { Packet.contents = test_str; dma_address = 6; dma_length = 12 }
      ];
  [%expect {|
    ("00000000  5a 00 09 20 71 75 69 63  6b 20 62 72              |Z.. quick br|")
    Cycles: 20
    ("00000000  5a 00 2b 54 68 65 20 71  75 69 63 6b 20 62 72 6f  |Z.+The quick bro|"
     "00000010  77 6e 20 66 6f 78 20 6a  75 6d 70 73 20 6f 76 65  |wn fox jumps ove|"
     "00000020  72 20 74 68 65 20 6c 61  7a 79 20 64 6f 67        |r the lazy dog|")
    Cycles: 78
    ("00000000  5a 00 01 72                                       |Z..r|")
    Cycles: 6
    ("00000000  5a 00 0c 69 63 6b 20 62  72 6f 77 6e 20 66 6f     |Z..ick brown fo|")
    Cycles: 26
    |}]
;;
