open! Core
open Hardcaml
module Test_util = Util
open Hardcaml_test_harness
open Hardcaml_risc_v_hart
open Hardcaml_memory_controller
open! Bits

let debug = false

module Hart_config = struct
  let register_width = Register_width.B32
  let num_registers = 32
  let design_frequency = 50_000_000_000
end

module Memory_controller = Bram_memory_controller.Make (struct
    let capacity_in_bytes = 128
    let num_write_channels = 1
    let num_read_channels = 1
    let address_width = 32
    let data_bus_width = 32
  end)

open Memory_controller.Memory_bus
module Prefetcher = Prefetcher.Make (Memory_controller.Memory_bus)

module Test_machine = struct
  open! Signal
  open! Always

  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; valid : 'a [@rtlprefix "input_"]
      ; address : 'a [@bits 32] [@rtlprefix "input_"]
      }
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t =
      { valid : 'a
      ; address : 'a [@bits 32]
      ; value : 'a [@bits 32]
      ; ready : 'a
      }
    [@@deriving hardcaml]
  end

  let create (scope : Scope.t) ({ I.clock; clear; valid; address } : _ I.t) =
    let read_bus = Read_bus.Dest.Of_always.wire zero in
    let read_response = Read_response.With_valid.Of_always.wire zero in
    let prefetcher =
      Prefetcher.hierarchical
        scope
        { Prefetcher.I.clock
        ; board_clear = clear
        ; hart_clear = clear
        ; valid
        ; aligned_address = address
        ; read_bus = Read_bus.Dest.Of_always.value read_bus
        ; read_response = Read_response.With_valid.Of_always.value read_response
        }
    in
    let controller =
      Memory_controller.hierarchical
        ~build_mode:Simulation
        ~priority_mode:Priority_order
        ~read_latency:4
        scope
        { Memory_controller.I.clock
        ; clear
        ; write_to_controller = [ Write_bus.Source.Of_signal.zero () ]
        ; read_to_controller = [ prefetcher.read_bus ]
        }
    in
    compile
      [ Read_bus.Dest.Of_always.assign
          read_bus
          (List.nth_exn controller.read_to_controller 0)
      ; Read_response.With_valid.Of_always.assign
          read_response
          (List.nth_exn controller.read_response 0)
      ];
    { O.valid = prefetcher.valid
    ; address = prefetcher.aligned_address
    ; value = prefetcher.value
    ; ready = prefetcher.ready
    }
  ;;
end

module Harness = Cyclesim_harness.Make (Test_machine.I) (Test_machine.O)

let waves_config = if debug then Waves_config.to_home_subdirectory () else No_waves

let create_sim f =
  Harness.run
    ~trace:`All_named
    ~create:Test_machine.create
    ~waves_config
    (fun ~inputs:_ ~outputs:_ sim -> f sim)
;;

let issue_load ~address sim =
  let inputs : _ Test_machine.I.t = Cyclesim.inputs sim in
  let outputs_before : _ Test_machine.O.t =
    Cyclesim.outputs ~clock_edge:Side.Before sim
  in
  let outputs : _ Test_machine.O.t = Cyclesim.outputs sim in
  inputs.valid := Bits.vdd;
  inputs.address := of_unsigned_int ~width:32 address;
  let rec loop_until_ready max =
    if max = 0 then raise_s [%message "BUG: Timed out"];
    Cyclesim.cycle sim;
    if Bits.to_bool !(outputs_before.ready) then () else loop_until_ready (max - 1)
  in
  loop_until_ready 50;
  inputs.valid := Bits.gnd;
  let rec loop_until_valid max =
    if max = 0 then raise_s [%message "BUG: Timed out"];
    Cyclesim.cycle sim;
    if
      Bits.to_bool !(outputs_before.valid)
      && Bits.to_int_trunc !(outputs_before.address) = address
    then ()
    else loop_until_valid (max - 1)
  in
  loop_until_valid 50;
  let outputs = Test_machine.O.map ~f:(fun t -> Bits.to_int_trunc !t) outputs in
  print_s [%message (outputs : int Test_machine.O.t)]
;;

let%expect_test "prefetcher basic test" =
  create_sim (fun sim ->
    let issue address =
      try issue_load ~address sim with
      | _ -> print_s [%message "BUG: Timed out or exception"]
    in
    (* Initialize the main memory to some known values for testing. *)
    Test_util.program_ram
      sim
      (Array.init
         ~f:(fun i -> Bits.of_unsigned_int ~width:8 (if i % 4 = 0 then i / 4 else 0))
         128);
    issue 0;
    [%expect {| (outputs ((valid 1) (address 1) (value 1) (ready 0))) |}];
    issue 1;
    [%expect {| (outputs ((valid 1) (address 1) (value 1) (ready 0))) |}];
    issue 2;
    [%expect {| (outputs ((valid 1) (address 2) (value 2) (ready 0))) |}];
    issue 3;
    [%expect {| (outputs ((valid 1) (address 3) (value 3) (ready 0))) |}];
    issue 0;
    [%expect {| (outputs ((valid 1) (address 1) (value 1) (ready 0))) |}])
;;
