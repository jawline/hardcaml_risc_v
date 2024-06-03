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
  let _outputs : _ Machine.O.t = Cyclesim.outputs sim in
  (* The fifo needs a clear cycle to initialize *)
  inputs.clear := vdd;
  Cyclesim.cycle sim;
  inputs.clear := gnd;
  if debug
  then Waveform.expect ~serialize_to:name ~display_width:150 ~display_height:100 waveform
;;

let%expect_test "test" =
  test ~name:"/tmp/test_memory_to_packet8" ~packet:"Hello world";
  [%expect.unreachable]
[@@expect.uncaught_exn
  {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  ("circuit input signal must have a port name (unassigned wire?)"
    (input_signal (wire (width 1) (data_in empty))))
  Raised at Base__Error.raise in file "src/error.ml" (inlined), line 9, characters 14-30
  Called from Base__Or_error.ok_exn in file "src/or_error.ml", line 107, characters 17-32
  Called from Hardcaml__Circuit.create_exn in file "src/circuit.ml", line 167, characters 15-56
  Called from Hardcaml__Circuit.With_interface.create_exn in file "src/circuit.ml", line 410, characters 6-82
  Called from Hardcaml__Cyclesim.With_interface.create in file "src/cyclesim.ml", line 117, characters 18-81
  Called from Hardcaml_io_controller_test__Test_memory_to_packet8.test.create_sim in file "io_controller/test/test_memory_to_packet8.ml", line 99, characters 4-161
  Called from Hardcaml_io_controller_test__Test_memory_to_packet8.(fun) in file "io_controller/test/test_memory_to_packet8.ml", line 118, characters 2-64
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 234, characters 12-19 |}]
;;
