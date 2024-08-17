open! Core
open Hardcaml
open Hardcaml_waveterm
open Hardcaml_memory_controller
open! Bits

module Memory_controller = Memory_controller.Make (struct
    let capacity_in_bytes = 128
    let num_channels = 1
    let address_width = 32
    let data_bus_width = 32
  end)

let create_sim () =
  let module Sim = Cyclesim.With_interface (Memory_controller.I) (Memory_controller.O) in
  Sim.create
    ~config:Cyclesim.Config.trace_all
    (Memory_controller.create
       (Scope.create ~auto_label_hierarchical_ports:true ~flatten_design:true ()))
;;

let rec write ~address ~value sim =
  let inputs : _ Memory_controller.I.t = Cyclesim.inputs sim in
  let ch_zero = List.nth_exn inputs.ch_to_controller 0 in
  ch_zero.valid := Bits.vdd;
  ch_zero.data.write := Bits.vdd;
  ch_zero.data.address := Bits.of_int ~width:32 address;
  ch_zero.data.write_data := Bits.of_int ~width:32 value;
  let outputs : _ Memory_controller.O.t = Cyclesim.outputs ~clock_edge:Before sim in
  let ch_zero_rx = List.nth_exn outputs.ch_to_controller 0 in
  Cyclesim.cycle sim;
  if Bits.to_bool !(ch_zero_rx.ready)
  then (
    ch_zero.valid := Bits.gnd;
    ch_zero.data.write := Bits.gnd)
  else write ~address ~value sim
;;

let rec read ~address sim =
  let inputs : _ Memory_controller.I.t = Cyclesim.inputs sim in
  let ch_zero = List.nth_exn inputs.ch_to_controller 0 in
  ch_zero.valid := Bits.vdd;
  ch_zero.data.address := Bits.of_int ~width:32 address;
  let outputs : _ Memory_controller.O.t = Cyclesim.outputs ~clock_edge:Before sim in
  let ch_zero_rx = List.nth_exn outputs.controller_to_ch 0 in
  Cyclesim.cycle sim;
  if Bits.to_bool !(ch_zero_rx.valid)
  then (
    ch_zero.valid := Bits.gnd;
    assert (not (Bits.to_bool !(ch_zero_rx.data.error)));
    Bits.to_int !(ch_zero_rx.data.read_data))
  else read ~address sim
;;

let read_and_assert ~address ~value sim =
  let result = read ~address sim in
  if result <> value
  then print_s [%message "BUG: Expected" (result : int) "received" (value : int)]
;;

let debug = true

let%expect_test "read/write" =
  let sim = create_sim () in
  let waveform, sim = Waveform.create sim in
  let random = Splittable_random.of_int 1 in
  Core.protect
    ~finally:(fun _ ->
      if debug then Waveform.Serialize.marshall waveform "/tmp/read_write" else ())
    ~f:(fun _ ->
      for _i = 0 to 1000 do
        let next =
          Splittable_random.int ~lo:Int.min_value ~hi:Int.max_value random land 0xFFFFFFFF
        in
        write ~address:0 ~value:next sim;
        read_and_assert ~address:0 ~value:next sim
      done;
      ());
  [%expect {| |}]
;;
