open! Core
open Hardcaml
module Test_util = Util
open Hardcaml_test_harness
open Hardcaml_risc_v_hart
open! Bits

let debug = false

module Memory_controller = Test_memory_controller
module Registers = Registers.Make (Example_hart)
open Memory_controller.Memory_bus
module Fetch = Fetch.Make (Example_hart) (Memory_controller.Memory_bus) (Registers)

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
      ; value : 'a [@bits 32]
      }
    [@@deriving hardcaml]
  end

  let create (scope : Scope.t) ({ I.clock; clear; valid; address } : _ I.t) =
    let read_bus = Read_bus.Dest.Of_always.wire zero in
    let read_response = Read_response.With_valid.Of_always.wire zero in
    let fetch =
      Fetch.hierarchical
        scope
        { Fetch.I.clock = { clock; clear }
        ; valid
        ; pc = address
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
        { Memory_controller.I.clock = { clock; clear }
        ; instruction = { read_to_controller = []; write_to_controller = [] }
        ; data =
            { write_to_controller = [ Write_bus.Source.Of_signal.zero () ]
            ; read_to_controller = [ fetch.read_bus ]
            }
        }
    in
    compile
      [ Read_bus.Dest.Of_always.assign
          read_bus
          (List.nth_exn controller.data.read_to_controller 0)
      ; Read_response.With_valid.Of_always.assign
          read_response
          (List.nth_exn controller.data.read_response 0)
      ];
    { O.valid = fetch.valid; value = fetch.instruction }
  ;;
end

module Harness = Cyclesim_harness.Make (Test_machine.I) (Test_machine.O)

let waves_config = Waves_config.to_home_subdirectory_when debug

let create_sim f =
  Harness.run
    ~trace:`All_named
    ~create:Test_machine.create
    ~waves_config
    (fun ~inputs:_ ~outputs:_ sim -> f sim)
;;

let clear sim =
  let inputs : _ Test_machine.I.t = Cyclesim.inputs sim in
  inputs.clear := Bits.vdd;
  Cyclesim.cycle ~n:10 sim;
  inputs.clear := Bits.gnd
;;

let issue_load ~address sim =
  let inputs : _ Test_machine.I.t = Cyclesim.inputs sim in
  let outputs_before : _ Test_machine.O.t =
    Cyclesim.outputs ~clock_edge:Side.Before sim
  in
  inputs.valid := Bits.vdd;
  inputs.address := of_unsigned_int ~width:32 address;
  Cyclesim.cycle sim;
  inputs.valid := Bits.gnd;
  let rec loop_until_valid max =
    if max = 0 then raise_s [%message "BUG: Timed out"];
    if Bits.to_bool !(outputs_before.valid)
    then ()
    else (
      Cyclesim.cycle sim;
      loop_until_valid (max - 1))
  in
  loop_until_valid 50;
  let print ~msg =
    let outputs = Test_machine.O.map ~f:(fun t -> Bits.to_int_trunc !t) outputs_before in
    print_s [%message msg (outputs : int Test_machine.O.t)]
  in
  print ~msg:"The cycle valid raised";
  ()
;;

let%expect_test "fetch basic test" =
  create_sim (fun sim ->
    clear sim;
    let issue address =
      try issue_load ~address sim with
      | _ -> print_s [%message "BUG: Timed out or exception"]
    in
    (* Initialize the main memory to some known values for testing. *)
    Test_util.program_ram
      sim
      (Array.init
         ~f:(fun i -> Bits.of_unsigned_int ~width:8 (if i % 4 = 0 then i else 0))
         128);
    issue 0;
    [%expect {| ("The cycle valid raised" (outputs ((valid 1) (value 0)))) |}];
    issue 4;
    [%expect {| ("The cycle valid raised" (outputs ((valid 1) (value 4)))) |}];
    issue 8;
    [%expect {| ("The cycle valid raised" (outputs ((valid 1) (value 8)))) |}];
    issue 12;
    [%expect {| ("The cycle valid raised" (outputs ((valid 1) (value 12)))) |}];
    issue 0;
    [%expect {| ("The cycle valid raised" (outputs ((valid 1) (value 0)))) |}];
    issue 0;
    [%expect {| ("The cycle valid raised" (outputs ((valid 1) (value 0)))) |}];
    issue 0;
    [%expect {| ("The cycle valid raised" (outputs ((valid 1) (value 0)))) |}];
    issue 4;
    [%expect {| ("The cycle valid raised" (outputs ((valid 1) (value 4)))) |}];
    issue 4;
    [%expect {| ("The cycle valid raised" (outputs ((valid 1) (value 4)))) |}];
    issue 12;
    [%expect {| ("The cycle valid raised" (outputs ((valid 1) (value 12)))) |}]);
  [%expect {| |}]
;;
