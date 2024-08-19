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
    let num_channels = 1
    let address_width = 32
    let data_bus_width = 32
  end)

module Load = Load.Make (Hart_config) (Memory_controller)

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
    [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t =
      { new_rd : 'a [@bits 32] [@rtlname "new_rd"]
      ; error : 'a
      ; finished : 'a
      ; controller_to_hart : 'a Memory_controller.Rx_bus.Tx.t
           [@rtlprefix "controller_to_hart"]
      ; hart_to_memory_controller : 'a Memory_controller.Tx_bus.Rx.t
           [@rtlprefix "hart_to_controller"]
      }
    [@@deriving sexp_of, hardcaml]
  end

  let create (scope : Scope.t) ({ I.clock; clear; enable; funct3; address } : _ I.t) =
    let memory_controller_to_hart = Memory_controller.Rx_bus.Tx.Of_always.wire zero in
    let hart_to_memory_controller = Memory_controller.Tx_bus.Rx.Of_always.wire zero in
    let load =
      Load.hierarchical
        ~instance:"load"
        scope
        { Load.I.clock
        ; clear
        ; enable
        ; funct3
        ; address
        ; memory_controller_to_hart =
            Memory_controller.Rx_bus.Tx.Of_always.value memory_controller_to_hart
        ; hart_to_memory_controller =
            Memory_controller.Tx_bus.Rx.Of_always.value hart_to_memory_controller
        }
    in
    let controller =
      Memory_controller.hierarchical
        ~instance:"memory_controller"
        scope
        { Memory_controller.I.clock
        ; clear
        ; ch_to_controller = [ load.hart_to_memory_controller ]
        }
    in
    compile
      [ Memory_controller.Rx_bus.Tx.Of_always.assign
          memory_controller_to_hart
          (List.nth_exn controller.controller_to_ch 0)
      ; Memory_controller.Tx_bus.Rx.Of_always.assign
          hart_to_memory_controller
          (List.nth_exn controller.ch_to_controller 0)
      ];
    { O.new_rd = load.new_rd
    ; O.error = load.error
    ; O.finished = load.finished
    ; controller_to_hart =
        Memory_controller.Rx_bus.Tx.Of_always.value memory_controller_to_hart
    ; hart_to_memory_controller =
        Memory_controller.Tx_bus.Rx.Of_always.value hart_to_memory_controller
    }
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
    {|
    (outputs
     ((new_rd 00000000000000000000000000000001) (error 0) (finished 0)
      (controller_to_hart
       ((valid 0)
        (data ((error 0) (read_data 00000000000000000000000000000001)))))
      (hart_to_memory_controller ((ready 1)))))
    |}];
  (try test ~address:4 ~funct3:(Funct3.Load.to_int Funct3.Load.Lw) sim with
   | _ -> print_s [%message "BUG: Timed out or exception"]);
  [%expect
    {|
      (outputs
       ((new_rd 00000000000000000000000000000010) (error 0) (finished 0)
        (controller_to_hart
         ((valid 0)
          (data ((error 0) (read_data 00000000000000000000000000000010)))))
        (hart_to_memory_controller ((ready 1))))) |}];
  (try test ~address:8 ~funct3:(Funct3.Load.to_int Funct3.Load.Lw) sim with
   | _ -> print_s [%message "BUG: Timed out or exception"]);
  [%expect
    {|
      (outputs
       ((new_rd 00000000000000000000000000000011) (error 0) (finished 0)
        (controller_to_hart
         ((valid 0)
          (data ((error 0) (read_data 00000000000000000000000000000011)))))
        (hart_to_memory_controller ((ready 1))))) |}];
  (try test ~address:12 ~funct3:(Funct3.Load.to_int Funct3.Load.Lw) sim with
   | _ -> print_s [%message "BUG: Timed out or exception"]);
  [%expect
    {|
      (outputs
       ((new_rd 00000000000000000000000000000100) (error 0) (finished 0)
        (controller_to_hart
         ((valid 0)
          (data ((error 0) (read_data 00000000000000000000000000000100)))))
        (hart_to_memory_controller ((ready 1))))) |}];
  (* Unaligned loads, we expect these to fail *)
  (try test ~address:1 ~funct3:(Funct3.Load.to_int Funct3.Load.Lw) sim with
   | _ -> print_s [%message "BUG: Timed out or exception"]);
  [%expect
    {|
      (outputs
       ((new_rd 00000000000000000000000000000100) (error 1) (finished 1)
        (controller_to_hart
         ((valid 0)
          (data ((error 0) (read_data 00000000000000000000000000000100)))))
        (hart_to_memory_controller ((ready 1))))) |}];
  (try test ~address:2 ~funct3:(Funct3.Load.to_int Funct3.Load.Lw) sim with
   | _ -> print_s [%message "BUG: Timed out or exception"]);
  [%expect
    {|
      (outputs
       ((new_rd 00000000000000000000000000000100) (error 1) (finished 1)
        (controller_to_hart
         ((valid 0)
          (data ((error 0) (read_data 00000000000000000000000000000100)))))
        (hart_to_memory_controller ((ready 1))))) |}];
  (try test ~address:3 ~funct3:(Funct3.Load.to_int Funct3.Load.Lw) sim with
   | _ -> print_s [%message "BUG: Timed out or exception"]);
  [%expect
    {|
      (outputs
       ((new_rd 00000000000000000000000000000100) (error 1) (finished 1)
        (controller_to_hart
         ((valid 0)
          (data ((error 0) (read_data 00000000000000000000000000000100)))))
        (hart_to_memory_controller ((ready 1))))) |}];
  (try test ~address:5 ~funct3:(Funct3.Load.to_int Funct3.Load.Lw) sim with
   | _ -> print_s [%message "BUG: Timed out or exception"]);
  [%expect
    {|
      (outputs
       ((new_rd 00000000000000000000000000000100) (error 1) (finished 1)
        (controller_to_hart
         ((valid 0)
          (data ((error 0) (read_data 00000000000000000000000000000100)))))
        (hart_to_memory_controller ((ready 1))))) |}];
  (try test ~address:6 ~funct3:(Funct3.Load.to_int Funct3.Load.Lw) sim with
   | _ -> print_s [%message "BUG: Timed out or exception"]);
  [%expect
    {|
      (outputs
       ((new_rd 00000000000000000000000000000100) (error 1) (finished 1)
        (controller_to_hart
         ((valid 0)
          (data ((error 0) (read_data 00000000000000000000000000000100)))))
        (hart_to_memory_controller ((ready 1))))) |}];
  (try test ~address:7 ~funct3:(Funct3.Load.to_int Funct3.Load.Lw) sim with
   | _ -> print_s [%message "BUG: Timed out or exception"]);
  [%expect
    {|
      (outputs
       ((new_rd 00000000000000000000000000000100) (error 1) (finished 1)
        (controller_to_hart
         ((valid 0)
          (data ((error 0) (read_data 00000000000000000000000000000100)))))
        (hart_to_memory_controller ((ready 1))))) |}];
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
         Bits.concat_msb [ Bits.of_int ~width:16 (i + 1); Bits.of_int ~width:16 (i + 2) ])
       32);
  let waveform, sim = Waveform.create sim in
  (* Aligned loads, we expect these to succeed. *)
  (try test ~address:0 ~funct3:(Funct3.Load.to_int Funct3.Load.Lh) sim with
   | _ -> print_s [%message "BUG: Timed out or exception"]);
  [%expect
    {|
    (outputs
     ((new_rd 00000000000000000000000000000010) (error 0) (finished 0)
      (controller_to_hart
       ((valid 0)
        (data ((error 0) (read_data 00000000000000010000000000000010)))))
      (hart_to_memory_controller ((ready 1)))))
    |}];
  (try test ~address:2 ~funct3:(Funct3.Load.to_int Funct3.Load.Lh) sim with
   | _ -> print_s [%message "BUG: Timed out or exception"]);
  [%expect
    {|
      (outputs
       ((new_rd 00000000000000000000000000000001) (error 0) (finished 0)
        (controller_to_hart
         ((valid 0)
          (data ((error 0) (read_data 00000000000000010000000000000010)))))
        (hart_to_memory_controller ((ready 1))))) |}];
  (try test ~address:4 ~funct3:(Funct3.Load.to_int Funct3.Load.Lh) sim with
   | _ -> print_s [%message "BUG: Timed out or exception"]);
  [%expect
    {|
      (outputs
       ((new_rd 00000000000000000000000000000100) (error 0) (finished 0)
        (controller_to_hart
         ((valid 0)
          (data ((error 0) (read_data 00000000000000110000000000000100)))))
        (hart_to_memory_controller ((ready 1))))) |}];
  (try test ~address:6 ~funct3:(Funct3.Load.to_int Funct3.Load.Lh) sim with
   | _ -> print_s [%message "BUG: Timed out or exception"]);
  [%expect
    {|
      (outputs
       ((new_rd 00000000000000000000000000000011) (error 0) (finished 0)
        (controller_to_hart
         ((valid 0)
          (data ((error 0) (read_data 00000000000000110000000000000100)))))
        (hart_to_memory_controller ((ready 1))))) |}];
  (* Unaligned loads, we expect these to fail *)
  (try test ~address:1 ~funct3:(Funct3.Load.to_int Funct3.Load.Lh) sim with
   | _ -> print_s [%message "BUG: Timed out or exception"]);
  [%expect
    {|
      (outputs
       ((new_rd 00000000000000000000000000000011) (error 1) (finished 1)
        (controller_to_hart
         ((valid 0)
          (data ((error 0) (read_data 00000000000000110000000000000100)))))
        (hart_to_memory_controller ((ready 1))))) |}];
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
         Bits.concat_msb
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
    {|
    (outputs
     ((new_rd 00000000000000000000000000000100) (error 0) (finished 0)
      (controller_to_hart
       ((valid 0)
        (data ((error 0) (read_data 00000001000000100000001100000100)))))
      (hart_to_memory_controller ((ready 1)))))
    |}];
  (try test ~address:1 ~funct3:(Funct3.Load.to_int Funct3.Load.Lb) sim with
   | _ -> print_s [%message "BUG: Timed out or exception"]);
  [%expect
    {|
      (outputs
       ((new_rd 00000000000000000000000000000011) (error 0) (finished 0)
        (controller_to_hart
         ((valid 0)
          (data ((error 0) (read_data 00000001000000100000001100000100)))))
        (hart_to_memory_controller ((ready 1))))) |}];
  (try test ~address:2 ~funct3:(Funct3.Load.to_int Funct3.Load.Lb) sim with
   | _ -> print_s [%message "BUG: Timed out or exception"]);
  [%expect
    {|
      (outputs
       ((new_rd 00000000000000000000000000000010) (error 0) (finished 0)
        (controller_to_hart
         ((valid 0)
          (data ((error 0) (read_data 00000001000000100000001100000100)))))
        (hart_to_memory_controller ((ready 1))))) |}];
  (try test ~address:3 ~funct3:(Funct3.Load.to_int Funct3.Load.Lb) sim with
   | _ -> print_s [%message "BUG: Timed out or exception"]);
  [%expect
    {|
      (outputs
       ((new_rd 00000000000000000000000000000001) (error 0) (finished 0)
        (controller_to_hart
         ((valid 0)
          (data ((error 0) (read_data 00000001000000100000001100000100)))))
        (hart_to_memory_controller ((ready 1))))) |}];
  (try test ~address:4 ~funct3:(Funct3.Load.to_int Funct3.Load.Lb) sim with
   | _ -> print_s [%message "BUG: Timed out or exception"]);
  [%expect
    {|
      (outputs
       ((new_rd 00000000000000000000000000001000) (error 0) (finished 0)
        (controller_to_hart
         ((valid 0)
          (data ((error 0) (read_data 00000101000001100000011100001000)))))
        (hart_to_memory_controller ((ready 1))))) |}];
  Waveform.Serialize.marshall waveform "/tmp/test_load_byte";
  [%expect {| |}]
;;

(* TODO: Sign extension tests *)
