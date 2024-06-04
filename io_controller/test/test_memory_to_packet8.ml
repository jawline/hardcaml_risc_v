open! Core
open Hardcaml
open Hardcaml_waveterm
open Hardcaml_io_controller
open Hardcaml_memory_controller
open! Bits

let debug = false

let write_packet_to_memory ~address ~packet sim =
  let ram = Cyclesim.lookup_mem sim "main_memory_bram" |> Option.value_exn in
  let packet = String.to_array packet in
  for i = 0 to address / 4 do
    let m = Array.get ram i in
    let start = i * 4 in
    let new_bits =
      [ packet.(start); packet.(start + 1); packet.(start + 2); packet.(start + 3) ]
      |> List.map ~f:Bits.of_char
      |> Bits.concat_lsb
    in
    Bits.Mutable.copy_bits ~src:new_bits ~dst:m
  done
;;

let test ~name ~packet =
  let module Packet =
    Packet.Make (struct
      let data_bus_width = 8
    end)
  in
  let module Memory_controller =
    Memory_controller.Make (struct
      let num_bytes = 128
      let num_channels = 1
      let address_width = 32
      let data_bus_width = 32
    end)
  in
  let module Memory_to_packet8 =
    Memory_to_packet8.Make
      (struct
        let magic = Some 'Q'
      end)
      (Memory_controller)
  in
  let module Machine = struct
    module I = struct
      type 'a t =
        { clock : 'a
        ; clear : 'a
        ; enable : 'a
        ; address : 'a [@bits 32]
        ; length : 'a [@bits 16]
        }
      [@@deriving sexp_of, hardcaml]
    end

    module O = Memory_to_packet8.O

    let create (scope : Scope.t) { I.clock; clear; enable; address; length } =
      let ch_to_controller = Memory_controller.Tx_bus.Tx.Of_always.wire Signal.zero in
      let controller_to_ch = Memory_controller.Rx_bus.Rx.Of_always.wire Signal.zero in
      let controller =
        Memory_controller.hierarchical
          ~instance:"memory_controller"
          scope
          { Memory_controller.I.clock
          ; clear
          ; ch_to_controller =
              [ Memory_controller.Tx_bus.Tx.Of_always.value ch_to_controller ]
          ; controller_to_ch =
              [ Memory_controller.Rx_bus.Rx.Of_always.value controller_to_ch ]
          }
      in
      let output =
        Memory_to_packet8.hierarchical
          ~instance:"packet8"
          scope
          { Memory_to_packet8.I.clock
          ; clear
          ; enable = { valid = enable; value = { address; length } }
          ; output_packet = { ready = Signal.vdd }
          ; memory = List.nth_exn controller.ch_to_controller 0
          ; memory_response = List.nth_exn controller.controller_to_ch 0
          }
      in
      Always.compile
        [ Memory_controller.Tx_bus.Tx.Of_always.assign ch_to_controller output.memory
        ; Memory_controller.Rx_bus.Rx.Of_always.assign
            controller_to_ch
            output.memory_response
        ];
      output
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
  write_packet_to_memory ~address:4 ~packet sim;
  let waveform, sim = Waveform.create sim in
  let inputs : _ Machine.I.t = Cyclesim.inputs sim in
  let outputs : _ Machine.O.t = Cyclesim.outputs sim in
  let data = ref "" in
  let store_outputs () =
    if Bits.to_bool !(outputs.output_packet.valid)
    then (
            data := String.concat [ !data ;  ((Bits.to_char !(outputs.output_packet.data.data))) |> Char.to_string ])
    else ()
  in
  let issue_read ~address ~length =
    inputs.enable := Bits.vdd;
    inputs.address := Bits.of_int ~width:32 address;
    inputs.length := Bits.of_int ~width:16 length;
    Cyclesim.cycle sim;
    store_outputs ();
    inputs.enable := Bits.gnd;
    let count = ref 0 in
    while !count <> 100 && not (Bits.to_bool !(outputs.output_packet.data.last)) do
      Cyclesim.cycle sim;
      store_outputs ();
      incr count
    done;
    print_s [%message "" ~_:((!data) : String.Hexdump.t)];
    printf "%i\n" !(count);
  in
  issue_read ~address:2 ~length:4;
  if debug
  then Waveform.expect ~serialize_to:name ~display_width:150 ~display_height:100 waveform
;;

let%expect_test "test" =
  test ~name:"/tmp/test_memory_to_packet8" ~packet:"Hello world";
  [%expect {|
    ("00000000  51 00 04 6c 6c                                    |Q..ll|")
    100 |}]
;;
