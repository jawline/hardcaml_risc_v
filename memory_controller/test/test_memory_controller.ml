open! Core
open Hardcaml
open Hardcaml_waveterm
open Hardcaml_memory_controller
open! Bits

module Make_tests (C : sig
    val num_channels : int
  end) =
struct
  module Memory_controller = Memory_controller.Make (struct
      let capacity_in_bytes = 128
      let num_channels = C.num_channels
      let address_width = 32
      let data_bus_width = 32
    end)

  let create_sim () =
    let module Sim = Cyclesim.With_interface (Memory_controller.I) (Memory_controller.O)
    in
    Sim.create
      ~config:Cyclesim.Config.trace_all
      (Memory_controller.create
         (Scope.create ~auto_label_hierarchical_ports:true ~flatten_design:true ()))
  ;;

  let rec write ~address ~value ~ch sim =
    let inputs : _ Memory_controller.I.t = Cyclesim.inputs sim in
    let ch_tx = List.nth_exn inputs.ch_to_controller ch in
    ch_tx.valid := Bits.vdd;
    ch_tx.data.write := Bits.vdd;
    ch_tx.data.address := Bits.of_int ~width:32 address;
    ch_tx.data.write_data := Bits.of_int ~width:32 value;
    let outputs : _ Memory_controller.O.t = Cyclesim.outputs ~clock_edge:Before sim in
    let ch_rx = List.nth_exn outputs.ch_to_controller ch in
    Cyclesim.cycle sim;
    if Bits.to_bool !(ch_rx.ready)
    then (
      List.iteri
        ~f:(fun i rx ->
          if i <> ch
          then (
            if Bits.to_bool !(rx.ready)
            then
              print_s [%message "BUG: We only expect one channel to have a ready signal."];
            ())
          else ())
        outputs.ch_to_controller;
      ch_tx.valid := Bits.gnd;
      ch_tx.data.write := Bits.gnd)
    else write ~address ~value ~ch sim
  ;;

  let rec read ~address ~ch sim =
    let inputs : _ Memory_controller.I.t = Cyclesim.inputs sim in
    let ch_tx = List.nth_exn inputs.ch_to_controller ch in
    ch_tx.valid := Bits.vdd;
    ch_tx.data.address := Bits.of_int ~width:32 address;
    let outputs : _ Memory_controller.O.t = Cyclesim.outputs ~clock_edge:Before sim in
    let ch_rx = List.nth_exn outputs.controller_to_ch ch in
    Cyclesim.cycle sim;
    if Bits.to_bool !(ch_rx.valid)
    then (
      ch_tx.valid := Bits.gnd;
      assert (not (Bits.to_bool !(ch_rx.data.error)));
      Bits.to_int !(ch_rx.data.read_data))
    else read ~address ~ch sim
  ;;

  let read_and_assert ~address ~value ~ch sim =
    let result = read ~address ~ch sim in
    if result <> value
    then print_s [%message "BUG: Expected" (result : int) "received" (value : int)]
  ;;

  let debug = false

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
            Splittable_random.int ~lo:Int.min_value ~hi:Int.max_value random
            land 0xFFFFFFFF
          in
          let ch = Splittable_random.int ~lo:0 ~hi:C.num_channels random in
          let address = Splittable_random.int ~lo:0 ~hi:127 random land lnot 0b11 in
          write ~address ~value:next ~ch sim;
          read_and_assert ~address ~value:next ~ch sim
        done;
        ());
    [%expect {| |}]
  ;;
end
