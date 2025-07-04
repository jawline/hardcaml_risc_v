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
    let data_bus_width = 128
  end)

open Memory_controller.Memory_bus
module Load = Load.Make (Hart_config) (Memory_controller.Memory_bus)

module Test_machine = struct
  open! Signal
  open! Always

  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; enable : 'a
      ; funct3 : 'a [@bits 3]
      ; address : 'a [@bits 32]
      }
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t =
      { new_rd : 'a [@bits 32] [@rtlname "new_rd"]
      ; error : 'a
      ; finished : 'a
      }
    [@@deriving hardcaml]
  end

  let create (scope : Scope.t) ({ I.clock; clear; enable; funct3; address } : _ I.t) =
    let read_bus = Read_bus.Dest.Of_always.wire zero in
    let read_response = Read_response.With_valid.Of_always.wire zero in
    let load =
      Load.hierarchical
        scope
        { Load.I.clock
        ; clear
        ; enable
        ; op =
            (let test_funct3 op = funct3 ==:. Funct3.Load.to_int op in
             Funct3.Load.Onehot.construct_onehot ~f:test_funct3)
        ; address
        ; read_bus = Read_bus.Dest.Of_always.value read_bus
        ; read_response = Read_response.With_valid.Of_always.value read_response
        }
    in
    let controller =
      Memory_controller.hierarchical
        ~build_mode:Simulation
        ~priority_mode:Priority_order
        ~read_latency:1
        scope
        { Memory_controller.I.clock
        ; clear
        ; write_to_controller = [ Write_bus.Source.Of_signal.zero () ]
        ; read_to_controller = [ load.read_bus ]
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
    { O.new_rd = load.new_rd; error = load.error; finished = load.finished }
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

let test ~address ~funct3 sim =
  let inputs : _ Test_machine.I.t = Cyclesim.inputs sim in
  let outputs_before : _ Test_machine.O.t =
    Cyclesim.outputs ~clock_edge:Side.Before sim
  in
  let outputs : _ Test_machine.O.t = Cyclesim.outputs sim in
  inputs.enable := Bits.vdd;
  inputs.address := of_unsigned_int ~width:32 address;
  inputs.funct3 := of_unsigned_int ~width:3 funct3;
  let rec loop_until_finished max =
    if max = 0 then raise_s [%message "BUG: Timed out"];
    Cyclesim.cycle sim;
    if Bits.to_bool !(outputs_before.finished) then () else loop_until_finished (max - 1)
  in
  loop_until_finished 50;
  let outputs = Test_machine.O.map ~f:(fun t -> Bits.to_int_trunc !t) outputs in
  print_s [%message (outputs : int Test_machine.O.t)]
;;

let%expect_test "lw" =
  create_sim (fun sim ->
    (* Initialize the main memory to some known values for testing. *)
    Test_util.program_ram
      sim
      (Array.init
         ~f:(fun i -> Bits.of_unsigned_int ~width:8 (if i % 4 = 0 then i / 4 else 0))
         128);
    (* Aligned loads, we expect these to succeed. *)
    (try test ~address:0 ~funct3:(Funct3.Load.to_int Funct3.Load.Lw) sim with
     | _ -> print_s [%message "BUG: Timed out or exception"]);
    [%expect {| (outputs ((new_rd 0) (error 0) (finished 0))) |}];
    (try test ~address:4 ~funct3:(Funct3.Load.to_int Funct3.Load.Lw) sim with
     | _ -> print_s [%message "BUG: Timed out or exception"]);
    [%expect {| (outputs ((new_rd 1) (error 0) (finished 0))) |}];
    (try test ~address:8 ~funct3:(Funct3.Load.to_int Funct3.Load.Lw) sim with
     | _ -> print_s [%message "BUG: Timed out or exception"]);
    [%expect {| (outputs ((new_rd 2) (error 0) (finished 0))) |}];
    (try test ~address:12 ~funct3:(Funct3.Load.to_int Funct3.Load.Lw) sim with
     | _ -> print_s [%message "BUG: Timed out or exception"]);
    [%expect {| (outputs ((new_rd 3) (error 0) (finished 0))) |}];
    (try test ~address:16 ~funct3:(Funct3.Load.to_int Funct3.Load.Lw) sim with
     | _ -> print_s [%message "BUG: Timed out or exception"]);
    [%expect {| (outputs ((new_rd 4) (error 0) (finished 0))) |}];
    (try test ~address:20 ~funct3:(Funct3.Load.to_int Funct3.Load.Lw) sim with
     | _ -> print_s [%message "BUG: Timed out or exception"]);
    [%expect {| (outputs ((new_rd 5) (error 0) (finished 0))) |}];
    (try test ~address:24 ~funct3:(Funct3.Load.to_int Funct3.Load.Lw) sim with
     | _ -> print_s [%message "BUG: Timed out or exception"]);
    [%expect {| (outputs ((new_rd 6) (error 0) (finished 0))) |}];
    (try test ~address:28 ~funct3:(Funct3.Load.to_int Funct3.Load.Lw) sim with
     | _ -> print_s [%message "BUG: Timed out or exception"]);
    [%expect {| (outputs ((new_rd 7) (error 0) (finished 0))) |}];
    (* Unaligned loads, we expect these to fail *)
    (try test ~address:1 ~funct3:(Funct3.Load.to_int Funct3.Load.Lw) sim with
     | _ -> print_s [%message "BUG: Timed out or exception"]);
    [%expect {| (outputs ((new_rd 4) (error 1) (finished 1))) |}];
    (try test ~address:2 ~funct3:(Funct3.Load.to_int Funct3.Load.Lw) sim with
     | _ -> print_s [%message "BUG: Timed out or exception"]);
    [%expect {| (outputs ((new_rd 4) (error 1) (finished 1))) |}];
    (try test ~address:3 ~funct3:(Funct3.Load.to_int Funct3.Load.Lw) sim with
     | _ -> print_s [%message "BUG: Timed out or exception"]);
    [%expect {| (outputs ((new_rd 4) (error 1) (finished 1))) |}];
    (try test ~address:5 ~funct3:(Funct3.Load.to_int Funct3.Load.Lw) sim with
     | _ -> print_s [%message "BUG: Timed out or exception"]);
    [%expect {| (outputs ((new_rd 5) (error 1) (finished 1))) |}];
    (try test ~address:6 ~funct3:(Funct3.Load.to_int Funct3.Load.Lw) sim with
     | _ -> print_s [%message "BUG: Timed out or exception"]);
    [%expect {| (outputs ((new_rd 5) (error 1) (finished 1))) |}];
    (try test ~address:7 ~funct3:(Funct3.Load.to_int Funct3.Load.Lw) sim with
     | _ -> print_s [%message "BUG: Timed out or exception"]);
    [%expect {| (outputs ((new_rd 5) (error 1) (finished 1))) |}]);
  [%expect {| |}]
;;

let%expect_test "lh" =
  create_sim (fun sim ->
    (* Initialize the main memory to some known values for testing. *)
    Test_util.program_ram
      sim
      (Array.init
         ~f:(fun i -> Bits.of_unsigned_int ~width:8 (if i % 2 = 0 then i / 2 else 0))
         128);
    (* Aligned loads, we expect these to succeed. *)
    (try test ~address:0 ~funct3:(Funct3.Load.to_int Funct3.Load.Lh) sim with
     | _ -> print_s [%message "BUG: Timed out or exception"]);
    [%expect {| (outputs ((new_rd 0) (error 0) (finished 0))) |}];
    (try test ~address:2 ~funct3:(Funct3.Load.to_int Funct3.Load.Lh) sim with
     | _ -> print_s [%message "BUG: Timed out or exception"]);
    [%expect {| (outputs ((new_rd 1) (error 0) (finished 0))) |}];
    (try test ~address:4 ~funct3:(Funct3.Load.to_int Funct3.Load.Lh) sim with
     | _ -> print_s [%message "BUG: Timed out or exception"]);
    [%expect {| (outputs ((new_rd 2) (error 0) (finished 0))) |}];
    (try test ~address:6 ~funct3:(Funct3.Load.to_int Funct3.Load.Lh) sim with
     | _ -> print_s [%message "BUG: Timed out or exception"]);
    [%expect {| (outputs ((new_rd 3) (error 0) (finished 0))) |}];
    (* Unaligned loads, we expect these to fail *)
    (try test ~address:1 ~funct3:(Funct3.Load.to_int Funct3.Load.Lh) sim with
     | _ -> print_s [%message "BUG: Timed out or exception"]);
    [%expect {| (outputs ((new_rd 0) (error 1) (finished 1))) |}]);
  [%expect {| |}]
;;

let%expect_test "lb" =
  create_sim (fun sim ->
    (* Initialize the main memory to some known values for testing. *)
    Test_util.program_ram
      sim
      (Array.init ~f:(fun i -> Bits.of_unsigned_int ~width:8 i) 128);
    (* Aligned loads, we expect these to succeed. *)
    (try test ~address:0 ~funct3:(Funct3.Load.to_int Funct3.Load.Lb) sim with
     | _ -> print_s [%message "BUG: Timed out or exception"]);
    [%expect {| (outputs ((new_rd 0) (error 0) (finished 0))) |}];
    (try test ~address:1 ~funct3:(Funct3.Load.to_int Funct3.Load.Lb) sim with
     | _ -> print_s [%message "BUG: Timed out or exception"]);
    [%expect {| (outputs ((new_rd 1) (error 0) (finished 0))) |}];
    (try test ~address:2 ~funct3:(Funct3.Load.to_int Funct3.Load.Lb) sim with
     | _ -> print_s [%message "BUG: Timed out or exception"]);
    [%expect {| (outputs ((new_rd 2) (error 0) (finished 0))) |}];
    (try test ~address:3 ~funct3:(Funct3.Load.to_int Funct3.Load.Lb) sim with
     | _ -> print_s [%message "BUG: Timed out or exception"]);
    [%expect {| (outputs ((new_rd 3) (error 0) (finished 0))) |}];
    (try test ~address:4 ~funct3:(Funct3.Load.to_int Funct3.Load.Lb) sim with
     | _ -> print_s [%message "BUG: Timed out or exception"]);
    [%expect {| (outputs ((new_rd 4) (error 0) (finished 0))) |}]);
  [%expect {| |}]
;;

(* TODO: Sign extension tests *)
