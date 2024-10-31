open! Core
open Hardcaml
module Test_util = Util
open Hardcaml_waveterm
open Hardcaml_risc_v_hart
open Hardcaml_memory_controller
open! Bits

module Hart_config = struct
  let register_width = Register_width.B32
  let num_registers = 32
end

module Memory_controller = Memory_controller.Make (struct
    let capacity_in_bytes = 128
    let num_write_channels = 1
    let num_read_channels = 1
    let address_width = 32
    let data_bus_width = 32
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
    let read_bus = Read_bus.Rx.Of_always.wire zero in
    let read_response = Read_response.With_valid.Of_always.wire zero in
    let load =
      Load.hierarchical
        ~instance:"load"
        scope
        { Load.I.clock
        ; clear
        ; enable
        ; funct3
        ; address
        ; read_bus = Read_bus.Rx.Of_always.value read_bus
        ; read_response = Read_response.With_valid.Of_always.value read_response
        }
    in
    let controller =
      Memory_controller.hierarchical
        ~instance:"memory_controller"
        scope
        { Memory_controller.I.clock
        ; clear
        ; write_to_controller = [ Write_bus.Tx.Of_signal.of_int 0 ]
        ; read_to_controller = [ load.read_bus ]
        }
    in
    compile
      [ Read_bus.Rx.Of_always.assign
          read_bus
          (List.nth_exn controller.read_to_controller 0)
      ; Read_response.With_valid.Of_always.assign
          read_response
          (List.nth_exn controller.read_response 0)
      ];
    { O.new_rd = load.new_rd; error = load.error; finished = load.finished }
  ;;
end

let create_sim () =
  let module Sim = Cyclesim.With_interface (Test_machine.I) (Test_machine.O) in
  Sim.create
    ~config:Cyclesim.Config.trace_all
    (Test_machine.create
       (Scope.create ~auto_label_hierarchical_ports:true ~flatten_design:true ()))
;;

let test ~address ~funct3 sim =
  let inputs : _ Test_machine.I.t = Cyclesim.inputs sim in
  let outputs_before : _ Test_machine.O.t =
    Cyclesim.outputs ~clock_edge:Side.Before sim
  in
  let outputs : _ Test_machine.O.t = Cyclesim.outputs sim in
  inputs.enable := Bits.vdd;
  inputs.address := of_int ~width:32 address;
  inputs.funct3 := of_int ~width:3 funct3;
  let rec loop_until_finished max =
    if max = 0 then raise_s [%message "BUG: Timed out"];
    Cyclesim.cycle sim;
    if Bits.to_bool !(outputs_before.finished) then () else loop_until_finished (max - 1)
  in
  loop_until_finished 50;
  print_s [%message (outputs : Bits.t ref Test_machine.O.t)]
;;

let%expect_test "lw" =
  let sim = create_sim () in
  (* Initialize the main memory to some known values for testing. *)
  Test_util.program_ram sim (Array.init ~f:(fun i -> Bits.of_int ~width:32 (i + 1)) 32);
  let waveform, sim = Waveform.create sim in
  (* Aligned loads, we expect these to succeed. *)
  (try test ~address:0 ~funct3:(Funct3.Load.to_int Funct3.Load.Lw) sim with
   | _ -> print_s [%message "BUG: Timed out or exception"]);
  [%expect
    {| (outputs ((new_rd 00000000000000000000000000000001) (error 0) (finished 0))) |}];
  (try test ~address:4 ~funct3:(Funct3.Load.to_int Funct3.Load.Lw) sim with
   | _ -> print_s [%message "BUG: Timed out or exception"]);
  [%expect
    {| (outputs ((new_rd 00000000000000000000000000000010) (error 0) (finished 0))) |}];
  (try test ~address:8 ~funct3:(Funct3.Load.to_int Funct3.Load.Lw) sim with
   | _ -> print_s [%message "BUG: Timed out or exception"]);
  [%expect
    {| (outputs ((new_rd 00000000000000000000000000000011) (error 0) (finished 0))) |}];
  (try test ~address:12 ~funct3:(Funct3.Load.to_int Funct3.Load.Lw) sim with
   | _ -> print_s [%message "BUG: Timed out or exception"]);
  [%expect
    {| (outputs ((new_rd 00000000000000000000000000000100) (error 0) (finished 0))) |}];
  (* Unaligned loads, we expect these to fail *)
  (try test ~address:1 ~funct3:(Funct3.Load.to_int Funct3.Load.Lw) sim with
   | _ -> print_s [%message "BUG: Timed out or exception"]);
  [%expect
    {| (outputs ((new_rd 00000000000000000000000000000100) (error 1) (finished 1))) |}];
  (try test ~address:2 ~funct3:(Funct3.Load.to_int Funct3.Load.Lw) sim with
   | _ -> print_s [%message "BUG: Timed out or exception"]);
  [%expect
    {| (outputs ((new_rd 00000000000000000000000000000100) (error 1) (finished 1))) |}];
  (try test ~address:3 ~funct3:(Funct3.Load.to_int Funct3.Load.Lw) sim with
   | _ -> print_s [%message "BUG: Timed out or exception"]);
  [%expect
    {| (outputs ((new_rd 00000000000000000000000000000100) (error 1) (finished 1))) |}];
  (try test ~address:5 ~funct3:(Funct3.Load.to_int Funct3.Load.Lw) sim with
   | _ -> print_s [%message "BUG: Timed out or exception"]);
  [%expect
    {| (outputs ((new_rd 00000000000000000000000000000100) (error 1) (finished 1))) |}];
  (try test ~address:6 ~funct3:(Funct3.Load.to_int Funct3.Load.Lw) sim with
   | _ -> print_s [%message "BUG: Timed out or exception"]);
  [%expect
    {| (outputs ((new_rd 00000000000000000000000000000100) (error 1) (finished 1))) |}];
  (try test ~address:7 ~funct3:(Funct3.Load.to_int Funct3.Load.Lw) sim with
   | _ -> print_s [%message "BUG: Timed out or exception"]);
  [%expect
    {| (outputs ((new_rd 00000000000000000000000000000100) (error 1) (finished 1))) |}];
  Waveform.Serialize.marshall waveform "/tmp/test_load";
  [%expect {| |}]
;;

let%expect_test "lh" =
  let sim = create_sim () in
  (* Initialize the main memory to some known values for testing. *)
  Test_util.program_ram
    sim
    (Array.init
       ~f:(fun i ->
         let i = i * 2 in
         Bits.concat_lsb [ Bits.of_int ~width:16 (i + 1); Bits.of_int ~width:16 (i + 2) ])
       32);
  let waveform, sim = Waveform.create sim in
  (* Aligned loads, we expect these to succeed. *)
  (try test ~address:0 ~funct3:(Funct3.Load.to_int Funct3.Load.Lh) sim with
   | _ -> print_s [%message "BUG: Timed out or exception"]);
  [%expect
    {| (outputs ((new_rd 00000000000000000000000000000001) (error 0) (finished 0))) |}];
  (try test ~address:2 ~funct3:(Funct3.Load.to_int Funct3.Load.Lh) sim with
   | _ -> print_s [%message "BUG: Timed out or exception"]);
  [%expect
    {| (outputs ((new_rd 00000000000000000000000000000010) (error 0) (finished 0))) |}];
  (try test ~address:4 ~funct3:(Funct3.Load.to_int Funct3.Load.Lh) sim with
   | _ -> print_s [%message "BUG: Timed out or exception"]);
  [%expect
    {| (outputs ((new_rd 00000000000000000000000000000011) (error 0) (finished 0))) |}];
  (try test ~address:6 ~funct3:(Funct3.Load.to_int Funct3.Load.Lh) sim with
   | _ -> print_s [%message "BUG: Timed out or exception"]);
  [%expect
    {| (outputs ((new_rd 00000000000000000000000000000100) (error 0) (finished 0))) |}];
  (* Unaligned loads, we expect these to fail *)
  (try test ~address:1 ~funct3:(Funct3.Load.to_int Funct3.Load.Lh) sim with
   | _ -> print_s [%message "BUG: Timed out or exception"]);
  [%expect
    {| (outputs ((new_rd 00000000000000000000000000000100) (error 1) (finished 1))) |}];
  Waveform.Serialize.marshall waveform "/tmp/test_load_half";
  [%expect {| |}]
;;

let%expect_test "lb" =
  let sim = create_sim () in
  (* Initialize the main memory to some known values for testing. *)
  Test_util.program_ram
    sim
    (Array.init
       ~f:(fun i ->
         let i = i * 4 in
         Bits.concat_lsb
           [ Bits.of_int ~width:8 (i + 1)
           ; Bits.of_int ~width:8 (i + 2)
           ; Bits.of_int ~width:8 (i + 3)
           ; Bits.of_int ~width:8 (i + 4)
           ])
       32);
  let waveform, sim = Waveform.create sim in
  (* Aligned loads, we expect these to succeed. *)
  (try test ~address:0 ~funct3:(Funct3.Load.to_int Funct3.Load.Lb) sim with
   | _ -> print_s [%message "BUG: Timed out or exception"]);
  [%expect
    {| (outputs ((new_rd 00000000000000000000000000000001) (error 0) (finished 0))) |}];
  (try test ~address:1 ~funct3:(Funct3.Load.to_int Funct3.Load.Lb) sim with
   | _ -> print_s [%message "BUG: Timed out or exception"]);
  [%expect
    {| (outputs ((new_rd 00000000000000000000000000000010) (error 0) (finished 0))) |}];
  (try test ~address:2 ~funct3:(Funct3.Load.to_int Funct3.Load.Lb) sim with
   | _ -> print_s [%message "BUG: Timed out or exception"]);
  [%expect
    {| (outputs ((new_rd 00000000000000000000000000000011) (error 0) (finished 0))) |}];
  (try test ~address:3 ~funct3:(Funct3.Load.to_int Funct3.Load.Lb) sim with
   | _ -> print_s [%message "BUG: Timed out or exception"]);
  [%expect
    {| (outputs ((new_rd 00000000000000000000000000000100) (error 0) (finished 0))) |}];
  (try test ~address:4 ~funct3:(Funct3.Load.to_int Funct3.Load.Lb) sim with
   | _ -> print_s [%message "BUG: Timed out or exception"]);
  [%expect
    {| (outputs ((new_rd 00000000000000000000000000000101) (error 0) (finished 0))) |}];
  Waveform.Serialize.marshall waveform "/tmp/test_load_byte";
  [%expect {| |}]
;;

(* TODO: Sign extension tests *)
