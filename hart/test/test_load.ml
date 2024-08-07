open! Core
open Hardcaml
open Hardcaml_waveterm
open Hardcaml_risc_v_hart
open Hardcaml_memory_controller
open! Bits

module Hart_config = struct
  let register_width = Register_width.B32
  let num_registers = 32
end

module Memory_controller = Memory_controller.Make (struct
    let num_bytes = 128
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
        ; controller_to_ch = [ load.memory_controller_to_hart ]
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
  let initial_ram = Cyclesim.lookup_mem sim "main_memory_bram" |> Option.value_exn in
  Array.iteri
    ~f:(fun i mut -> Bits.Mutable.copy_bits ~src:(Bits.of_int ~width:32 (i + 1)) ~dst:mut)
    initial_ram;
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
        (hart_to_memory_controller ((ready 1))))) |}];
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
  Waveform.expect
    ~serialize_to:"/tmp/test_load"
    ~display_width:150
    ~display_height:100
    waveform;
  [%expect
    {|
    ┌Signals───────────┐┌Waves───────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┐
    │clock             ││┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   │
    │                  ││    └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───│
    │clear             ││                                                                                                                                │
    │                  ││────────────────────────────────────────────────────────────────────────────────────────────────────────────────                │
    │enable            ││────────────────────────────────────────────────────────────────────────────────────────────────────────────────                │
    │                  ││                                                                                                                                │
    │                  ││────────────────┬───────────────┬───────────────┬───────────────┬───────┬───────┬───────┬───────┬───────┬───────                │
    │address           ││ 00000000       │00000004       │00000008       │0000000C       │000000.│000000.│000000.│000000.│000000.│000000.                │
    │                  ││────────────────┴───────────────┴───────────────┴───────────────┴───────┴───────┴───────┴───────┴───────┴───────                │
    │                  ││────────────────────────────────────────────────────────────────────────────────────────────────────────────────                │
    │funct3            ││ 2                                                                                                                              │
    │                  ││────────────────────────────────────────────────────────────────────────────────────────────────────────────────                │
    │controller_to_hart││                                                                                                                                │
    │                  ││────────────────────────────────────────────────────────────────────────────────────────────────────────────────                │
    │                  ││────────┬───────────────┬───────────────┬───────────────┬───────────────────────────────────────────────────────                │
    │controller_to_hart││ 000000.│00000001       │00000002       │00000003       │00000004                                                               │
    │                  ││────────┴───────────────┴───────────────┴───────────────┴───────────────────────────────────────────────────────                │
    │controller_to_hart││        ┌───────┐       ┌───────┐       ┌───────┐       ┌───────┐                                                               │
    │                  ││────────┘       └───────┘       └───────┘       └───────┘       └───────────────────────────────────────────────                │
    │error             ││                                                                ┌───────────────────────────────────────────────                │
    │                  ││────────────────────────────────────────────────────────────────┘                                                               │
    │finished          ││        ┌───────┐       ┌───────┐       ┌───────┐       ┌───────────────────────────────────────────────────────                │
    │                  ││────────┘       └───────┘       └───────┘       └───────┘                                                                       │
    │hart_to_controller││────────────────────────────────────────────────────────────────────────────────────────────────────────────────                │
    │                  ││                                                                                                                                │
    │                  ││────────┬───────────────┬───────────────┬───────────────┬───────────────────────────────────────────────────────                │
    │new_rd            ││ 000000.│00000001       │00000002       │00000003       │00000004                                                               │
    │                  ││────────┴───────────────┴───────────────┴───────────────┴───────────────────────────────────────────────────────                │
    │gnd               ││                                                                                                                                │
    │                  ││────────────────────────────────────────────────────────────────────────────────────────────────────────────────                │
    │                  ││────────────────┬───────────────┬───────────────┬───────────────┬───────────────────────┬───────────────────────                │
    │load$aligned_addre││ 00000000       │00000004       │00000008       │0000000C       │00000000               │00000004                               │
    │                  ││────────────────┴───────────────┴───────────────┴───────────────┴───────────────────────┴───────────────────────                │
    │                  ││────────────────────────────────────────────────────────────────┬───────┬───────┬───────┬───────┬───────┬───────                │
    │load$alignment_bit││ 00000000                                                       │000000.│000000.│000000.│000000.│000000.│000000.                │
    │                  ││────────────────────────────────────────────────────────────────┴───────┴───────┴───────┴───────┴───────┴───────                │
    │                  ││────────┬───────────────┬───────────────┬───────────────┬───────┬───────────────────────────────────────────────                │
    │load$byte         ││ 00     │01             │02             │03             │04     │00                                                             │
    │                  ││────────┴───────────────┴───────────────┴───────────────┴───────┴───────────────────────────────────────────────                │
    │load$current_state││        ┌───────┐       ┌───────┐       ┌───────┐       ┌───────┐                                                               │
    │                  ││────────┘       └───────┘       └───────┘       └───────┘       └───────────────────────────────────────────────                │
    │                  ││────────┬───────────────┬───────────────┬───────────────┬───────────────────────────────────────────────────────                │
    │load$full_word    ││ 000000.│00000001       │00000002       │00000003       │00000004                                                               │
    │                  ││────────┴───────────────┴───────────────┴───────────────┴───────────────────────────────────────────────────────                │
    │load$funct3_is_err││                                                                                                                                │
    │                  ││────────────────────────────────────────────────────────────────────────────────────────────────────────────────                │
    │                  ││────────┬───────────────┬───────────────┬───────────────┬───────┬───────────────────────────────────────────────                │
    │load$half_word    ││ 0000   │0001           │0002           │0003           │0004   │0000                                                           │
    │                  ││────────┴───────────────┴───────────────┴───────────────┴───────┴───────────────────────────────────────────────                │
    │                  ││────────────────┬───────────────┬───────────────┬───────────────┬───────┬───────┬───────┬───────┬───────┬───────                │
    │load$i$address    ││ 00000000       │00000004       │00000008       │0000000C       │000000.│000000.│000000.│000000.│000000.│000000.                │
    │                  ││────────────────┴───────────────┴───────────────┴───────────────┴───────┴───────┴───────┴───────┴───────┴───────                │
    │load$i$clear      ││                                                                                                                                │
    │                  ││────────────────────────────────────────────────────────────────────────────────────────────────────────────────                │
    │load$i$clock      ││                                                                                                                                │
    │                  ││────────────────────────────────────────────────────────────────────────────────────────────────────────────────                │
    │load$i$enable     ││────────────────────────────────────────────────────────────────────────────────────────────────────────────────                │
    │                  ││                                                                                                                                │
    │                  ││────────────────────────────────────────────────────────────────────────────────────────────────────────────────                │
    │load$i$funct3     ││ 2                                                                                                                              │
    │                  ││────────────────────────────────────────────────────────────────────────────────────────────────────────────────                │
    │load$i$hart_to_mem││────────────────────────────────────────────────────────────────────────────────────────────────────────────────                │
    │                  ││                                                                                                                                │
    │load$i$memory_cont││                                                                                                                                │
    │                  ││────────────────────────────────────────────────────────────────────────────────────────────────────────────────                │
    │                  ││────────┬───────────────┬───────────────┬───────────────┬───────────────────────────────────────────────────────                │
    │load$i$memory_cont││ 000000.│00000001       │00000002       │00000003       │00000004                                                               │
    │                  ││────────┴───────────────┴───────────────┴───────────────┴───────────────────────────────────────────────────────                │
    │load$i$memory_cont││        ┌───────┐       ┌───────┐       ┌───────┐       ┌───────┐                                                               │
    │                  ││────────┘       └───────┘       └───────┘       └───────┘       └───────────────────────────────────────────────                │
    │load$inputs_are_er││                                                                                                                                │
    │                  ││────────────────────────────────────────────────────────────────────────────────────────────────────────────────                │
    │load$is_unaligned ││                                                                ┌───────────────────────────────────────────────                │
    │                  ││────────────────────────────────────────────────────────────────┘                                                               │
    │load$o$error      ││                                                                ┌───────────────────────────────────────────────                │
    │                  ││────────────────────────────────────────────────────────────────┘                                                               │
    │load$o$finished   ││        ┌───────┐       ┌───────┐       ┌───────┐       ┌───────────────────────────────────────────────────────                │
    │                  ││────────┘       └───────┘       └───────┘       └───────┘                                                                       │
    │                  ││────────────────┬───────┬───────┬───────┬───────┬───────┬───────────────────────────────────────────────────────                │
    │load$o$hart_to_mem││ 00000000       │000000.│000000.│000000.│000000.│000000.│00000000                                                               │
    │                  ││────────────────┴───────┴───────┴───────┴───────┴───────┴───────────────────────────────────────────────────────                │
    │load$o$hart_to_mem││────────┐       ┌───────┐       ┌───────┐       ┌───────┐                                                                       │
    │                  ││        └───────┘       └───────┘       └───────┘       └───────────────────────────────────────────────────────                │
    │load$o$hart_to_mem││                                                                                                                                │
    │                  ││────────────────────────────────────────────────────────────────────────────────────────────────────────────────                │
    │                  ││────────────────────────────────────────────────────────────────────────────────────────────────────────────────                │
    │load$o$hart_to_mem││ 00000000                                                                                                                       │
    │                  ││────────────────────────────────────────────────────────────────────────────────────────────────────────────────                │
    │                  ││────────┬───────────────┬───────────────┬───────────────┬───────────────────────────────────────────────────────                │
    │load$o$new_rd     ││ 000000.│00000001       │00000002       │00000003       │00000004                                                               │
    │                  ││────────┴───────────────┴───────────────┴───────────────┴───────────────────────────────────────────────────────                │
    │                  ││────────────────────────────────────────────────────────────────┬───────┬───────┬───────┬───────┬───────┬───────                │
    │load$unaligned_bit││ 0                                                              │1      │2      │3      │1      │2      │3                      │
    │                  ││────────────────────────────────────────────────────────────────┴───────┴───────┴───────┴───────┴───────┴───────                │
    │                  ││────────────────────────────────────────────────────────────────────────────────────────────────────────────────                │
    │main_memory_bram  ││ 00000000                                                                                                                       │
    │                  ││────────────────────────────────────────────────────────────────────────────────────────────────────────────────                │
    │                  ││────────────────┬───────┬───────┬───────┬───────┬───────┬───────────────────────────────────────────────────────                │
    └──────────────────┘└────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┘
    670cae12ad2e6448508f2472fb2bd337 |}]
;;

let%expect_test "lh" =
  let sim = create_sim () in
  (* Initialize the main memory to some known values for testing. *)
  let initial_ram = Cyclesim.lookup_mem sim "main_memory_bram" |> Option.value_exn in
  Array.iteri
    ~f:(fun i mut ->
      let i = i * 2 in
      let test_value =
        Bits.concat_msb [ Bits.of_int ~width:16 (i + 1); Bits.of_int ~width:16 (i + 2) ]
      in
      Bits.Mutable.copy_bits ~src:test_value ~dst:mut)
    initial_ram;
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
        (hart_to_memory_controller ((ready 1))))) |}];
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
  Waveform.expect
    ~serialize_to:"/tmp/test_load_half"
    ~display_width:150
    ~display_height:100
    waveform;
  [%expect
    {|
    ┌Signals───────────┐┌Waves───────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┐
    │clock             ││┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   │
    │                  ││    └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───│
    │clear             ││                                                                                                                                │
    │                  ││────────────────────────────────────────────────────────────────────────                                                        │
    │enable            ││────────────────────────────────────────────────────────────────────────                                                        │
    │                  ││                                                                                                                                │
    │                  ││────────────────┬───────────────┬───────────────┬───────────────┬───────                                                        │
    │address           ││ 00000000       │00000002       │00000004       │00000006       │000000.                                                        │
    │                  ││────────────────┴───────────────┴───────────────┴───────────────┴───────                                                        │
    │                  ││────────────────────────────────────────────────────────────────────────                                                        │
    │funct3            ││ 1                                                                                                                              │
    │                  ││────────────────────────────────────────────────────────────────────────                                                        │
    │controller_to_hart││                                                                                                                                │
    │                  ││────────────────────────────────────────────────────────────────────────                                                        │
    │                  ││────────┬───────────────────────────────┬───────────────────────────────                                                        │
    │controller_to_hart││ 000000.│00010002                       │00030004                                                                               │
    │                  ││────────┴───────────────────────────────┴───────────────────────────────                                                        │
    │controller_to_hart││        ┌───────┐       ┌───────┐       ┌───────┐       ┌───────┐                                                               │
    │                  ││────────┘       └───────┘       └───────┘       └───────┘       └───────                                                        │
    │error             ││                                                                ┌───────                                                        │
    │                  ││────────────────────────────────────────────────────────────────┘                                                               │
    │finished          ││        ┌───────┐       ┌───────┐       ┌───────┐       ┌───────────────                                                        │
    │                  ││────────┘       └───────┘       └───────┘       └───────┘                                                                       │
    │hart_to_controller││────────────────────────────────────────────────────────────────────────                                                        │
    │                  ││                                                                                                                                │
    │                  ││────────┬───────┬───────────────┬───────┬───────┬───────────────────────                                                        │
    │new_rd            ││ 000000.│000000.│00000001       │000000.│000000.│00000003                                                                       │
    │                  ││────────┴───────┴───────────────┴───────┴───────┴───────────────────────                                                        │
    │gnd               ││                                                                                                                                │
    │                  ││────────────────────────────────────────────────────────────────────────                                                        │
    │                  ││────────────────────────────────┬───────────────────────────────┬───────                                                        │
    │load$aligned_addre││ 00000000                       │00000004                       │000000.                                                        │
    │                  ││────────────────────────────────┴───────────────────────────────┴───────                                                        │
    │                  ││────────────────┬───────────────┬───────────────┬───────────────┬───────                                                        │
    │load$alignment_bit││ 00000000       │00000002       │00000000       │00000002       │000000.                                                        │
    │                  ││────────────────┴───────────────┴───────────────┴───────────────┴───────                                                        │
    │                  ││────────┬───────┬───────────────┬───────┬───────┬───────────────┬───────                                                        │
    │load$byte         ││ 00     │02     │01             │02     │04     │03             │00                                                             │
    │                  ││────────┴───────┴───────────────┴───────┴───────┴───────────────┴───────                                                        │
    │load$current_state││        ┌───────┐       ┌───────┐       ┌───────┐       ┌───────┐                                                               │
    │                  ││────────┘       └───────┘       └───────┘       └───────┘       └───────                                                        │
    │                  ││────────┬───────────────────────────────┬───────────────────────────────                                                        │
    │load$full_word    ││ 000000.│00010002                       │00030004                                                                               │
    │                  ││────────┴───────────────────────────────┴───────────────────────────────                                                        │
    │load$funct3_is_err││                                                                                                                                │
    │                  ││────────────────────────────────────────────────────────────────────────                                                        │
    │                  ││────────┬───────┬───────────────┬───────┬───────┬───────────────────────                                                        │
    │load$half_word    ││ 0000   │0002   │0001           │0002   │0004   │0003                                                                           │
    │                  ││────────┴───────┴───────────────┴───────┴───────┴───────────────────────                                                        │
    │                  ││────────────────┬───────────────┬───────────────┬───────────────┬───────                                                        │
    │load$i$address    ││ 00000000       │00000002       │00000004       │00000006       │000000.                                                        │
    │                  ││────────────────┴───────────────┴───────────────┴───────────────┴───────                                                        │
    │load$i$clear      ││                                                                                                                                │
    │                  ││────────────────────────────────────────────────────────────────────────                                                        │
    │load$i$clock      ││                                                                                                                                │
    │                  ││────────────────────────────────────────────────────────────────────────                                                        │
    │load$i$enable     ││────────────────────────────────────────────────────────────────────────                                                        │
    │                  ││                                                                                                                                │
    │                  ││────────────────────────────────────────────────────────────────────────                                                        │
    │load$i$funct3     ││ 1                                                                                                                              │
    │                  ││────────────────────────────────────────────────────────────────────────                                                        │
    │load$i$hart_to_mem││────────────────────────────────────────────────────────────────────────                                                        │
    │                  ││                                                                                                                                │
    │load$i$memory_cont││                                                                                                                                │
    │                  ││────────────────────────────────────────────────────────────────────────                                                        │
    │                  ││────────┬───────────────────────────────┬───────────────────────────────                                                        │
    │load$i$memory_cont││ 000000.│00010002                       │00030004                                                                               │
    │                  ││────────┴───────────────────────────────┴───────────────────────────────                                                        │
    │load$i$memory_cont││        ┌───────┐       ┌───────┐       ┌───────┐       ┌───────┐                                                               │
    │                  ││────────┘       └───────┘       └───────┘       └───────┘       └───────                                                        │
    │load$inputs_are_er││                                                                                                                                │
    │                  ││────────────────────────────────────────────────────────────────────────                                                        │
    │load$is_unaligned ││                                                                ┌───────                                                        │
    │                  ││────────────────────────────────────────────────────────────────┘                                                               │
    │load$o$error      ││                                                                ┌───────                                                        │
    │                  ││────────────────────────────────────────────────────────────────┘                                                               │
    │load$o$finished   ││        ┌───────┐       ┌───────┐       ┌───────┐       ┌───────────────                                                        │
    │                  ││────────┘       └───────┘       └───────┘       └───────┘                                                                       │
    │                  ││────────────────────────────────┬───────┬───────┬───────┬───────────────                                                        │
    │load$o$hart_to_mem││ 00000000                       │000000.│000000.│000000.│00000000                                                               │
    │                  ││────────────────────────────────┴───────┴───────┴───────┴───────────────                                                        │
    │load$o$hart_to_mem││────────┐       ┌───────┐       ┌───────┐       ┌───────┐                                                                       │
    │                  ││        └───────┘       └───────┘       └───────┘       └───────────────                                                        │
    │load$o$hart_to_mem││                                                                                                                                │
    │                  ││────────────────────────────────────────────────────────────────────────                                                        │
    │                  ││────────────────────────────────────────────────────────────────────────                                                        │
    │load$o$hart_to_mem││ 00000000                                                                                                                       │
    │                  ││────────────────────────────────────────────────────────────────────────                                                        │
    │                  ││────────┬───────┬───────────────┬───────┬───────┬───────────────────────                                                        │
    │load$o$new_rd     ││ 000000.│000000.│00000001       │000000.│000000.│00000003                                                                       │
    │                  ││────────┴───────┴───────────────┴───────┴───────┴───────────────────────                                                        │
    │                  ││────────────────────────────────────────────────────────────────┬───────                                                        │
    │load$unaligned_bit││ 0                                                              │1                                                              │
    │                  ││────────────────────────────────────────────────────────────────┴───────                                                        │
    │                  ││────────────────────────────────────────────────────────────────────────                                                        │
    │main_memory_bram  ││ 00000000                                                                                                                       │
    │                  ││────────────────────────────────────────────────────────────────────────                                                        │
    │                  ││────────────────────────────────┬───────┬───────┬───────┬───────────────                                                        │
    └──────────────────┘└────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┘
    78e0e67b6fbbe52cad1a004dd47e30e4 |}]
;;

let%expect_test "lb" =
  let sim = create_sim () in
  (* Initialize the main memory to some known values for testing. *)
  let initial_ram = Cyclesim.lookup_mem sim "main_memory_bram" |> Option.value_exn in
  Array.iteri
    ~f:(fun i mut ->
      let i = i * 4 in
      let test_value =
        Bits.concat_msb
          [ Bits.of_int ~width:8 (i + 1)
          ; Bits.of_int ~width:8 (i + 2)
          ; Bits.of_int ~width:8 (i + 3)
          ; Bits.of_int ~width:8 (i + 4)
          ]
      in
      Bits.Mutable.copy_bits ~src:test_value ~dst:mut)
    initial_ram;
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
        (hart_to_memory_controller ((ready 1))))) |}];
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
  Waveform.expect
    ~serialize_to:"/tmp/test_load_byte"
    ~display_width:150
    ~display_height:100
    waveform;
  [%expect
    {|
    ┌Signals───────────┐┌Waves───────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┐
    │clock             ││┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   │
    │                  ││    └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───│
    │clear             ││                                                                                                                                │
    │                  ││────────────────────────────────────────────────────────────────────────────────                                                │
    │enable            ││────────────────────────────────────────────────────────────────────────────────                                                │
    │                  ││                                                                                                                                │
    │                  ││────────────────┬───────────────┬───────────────┬───────────────┬───────────────                                                │
    │address           ││ 00000000       │00000001       │00000002       │00000003       │00000004                                                       │
    │                  ││────────────────┴───────────────┴───────────────┴───────────────┴───────────────                                                │
    │                  ││────────────────────────────────────────────────────────────────────────────────                                                │
    │funct3            ││ 0                                                                                                                              │
    │                  ││────────────────────────────────────────────────────────────────────────────────                                                │
    │controller_to_hart││                                                                                                                                │
    │                  ││────────────────────────────────────────────────────────────────────────────────                                                │
    │                  ││────────┬───────────────────────────────────────────────────────────────┬───────                                                │
    │controller_to_hart││ 000000.│01020304                                                       │050607.                                                │
    │                  ││────────┴───────────────────────────────────────────────────────────────┴───────                                                │
    │controller_to_hart││        ┌───────┐       ┌───────┐       ┌───────┐       ┌───────┐       ┌───────                                                │
    │                  ││────────┘       └───────┘       └───────┘       └───────┘       └───────┘                                                       │
    │error             ││                                                                                                                                │
    │                  ││────────────────────────────────────────────────────────────────────────────────                                                │
    │finished          ││        ┌───────┐       ┌───────┐       ┌───────┐       ┌───────┐       ┌───────                                                │
    │                  ││────────┘       └───────┘       └───────┘       └───────┘       └───────┘                                                       │
    │hart_to_controller││────────────────────────────────────────────────────────────────────────────────                                                │
    │                  ││                                                                                                                                │
    │                  ││────────┬───────┬───────────────┬───────────────┬───────────────┬───────┬───────                                                │
    │new_rd            ││ 000000.│000000.│00000003       │00000002       │00000001       │000000.│000000.                                                │
    │                  ││────────┴───────┴───────────────┴───────────────┴───────────────┴───────┴───────                                                │
    │gnd               ││                                                                                                                                │
    │                  ││────────────────────────────────────────────────────────────────────────────────                                                │
    │                  ││────────────────────────────────────────────────────────────────┬───────────────                                                │
    │load$aligned_addre││ 00000000                                                       │00000004                                                       │
    │                  ││────────────────────────────────────────────────────────────────┴───────────────                                                │
    │                  ││────────────────┬───────────────┬───────────────┬───────────────┬───────────────                                                │
    │load$alignment_bit││ 00000000       │00000001       │00000002       │00000003       │00000000                                                       │
    │                  ││────────────────┴───────────────┴───────────────┴───────────────┴───────────────                                                │
    │                  ││────────┬───────┬───────────────┬───────────────┬───────────────┬───────┬───────                                                │
    │load$byte         ││ 00     │04     │03             │02             │01             │04     │08                                                     │
    │                  ││────────┴───────┴───────────────┴───────────────┴───────────────┴───────┴───────                                                │
    │load$current_state││        ┌───────┐       ┌───────┐       ┌───────┐       ┌───────┐       ┌───────                                                │
    │                  ││────────┘       └───────┘       └───────┘       └───────┘       └───────┘                                                       │
    │                  ││────────┬───────────────────────────────────────────────────────────────┬───────                                                │
    │load$full_word    ││ 000000.│01020304                                                       │050607.                                                │
    │                  ││────────┴───────────────────────────────────────────────────────────────┴───────                                                │
    │load$funct3_is_err││                                                                                                                                │
    │                  ││────────────────────────────────────────────────────────────────────────────────                                                │
    │                  ││────────┬───────┬───────────────────────────────────────────────┬───────┬───────                                                │
    │load$half_word    ││ 0000   │0304   │0102                                           │0304   │0708                                                   │
    │                  ││────────┴───────┴───────────────────────────────────────────────┴───────┴───────                                                │
    │                  ││────────────────┬───────────────┬───────────────┬───────────────┬───────────────                                                │
    │load$i$address    ││ 00000000       │00000001       │00000002       │00000003       │00000004                                                       │
    │                  ││────────────────┴───────────────┴───────────────┴───────────────┴───────────────                                                │
    │load$i$clear      ││                                                                                                                                │
    │                  ││────────────────────────────────────────────────────────────────────────────────                                                │
    │load$i$clock      ││                                                                                                                                │
    │                  ││────────────────────────────────────────────────────────────────────────────────                                                │
    │load$i$enable     ││────────────────────────────────────────────────────────────────────────────────                                                │
    │                  ││                                                                                                                                │
    │                  ││────────────────────────────────────────────────────────────────────────────────                                                │
    │load$i$funct3     ││ 0                                                                                                                              │
    │                  ││────────────────────────────────────────────────────────────────────────────────                                                │
    │load$i$hart_to_mem││────────────────────────────────────────────────────────────────────────────────                                                │
    │                  ││                                                                                                                                │
    │load$i$memory_cont││                                                                                                                                │
    │                  ││────────────────────────────────────────────────────────────────────────────────                                                │
    │                  ││────────┬───────────────────────────────────────────────────────────────┬───────                                                │
    │load$i$memory_cont││ 000000.│01020304                                                       │050607.                                                │
    │                  ││────────┴───────────────────────────────────────────────────────────────┴───────                                                │
    │load$i$memory_cont││        ┌───────┐       ┌───────┐       ┌───────┐       ┌───────┐       ┌───────                                                │
    │                  ││────────┘       └───────┘       └───────┘       └───────┘       └───────┘                                                       │
    │load$inputs_are_er││                                                                                                                                │
    │                  ││────────────────────────────────────────────────────────────────────────────────                                                │
    │load$is_unaligned ││                                                                                                                                │
    │                  ││────────────────────────────────────────────────────────────────────────────────                                                │
    │load$o$error      ││                                                                                                                                │
    │                  ││────────────────────────────────────────────────────────────────────────────────                                                │
    │load$o$finished   ││        ┌───────┐       ┌───────┐       ┌───────┐       ┌───────┐       ┌───────                                                │
    │                  ││────────┘       └───────┘       └───────┘       └───────┘       └───────┘                                                       │
    │                  ││────────────────────────────────────────────────────────────────┬───────┬───────                                                │
    │load$o$hart_to_mem││ 00000000                                                       │000000.│000000.                                                │
    │                  ││────────────────────────────────────────────────────────────────┴───────┴───────                                                │
    │load$o$hart_to_mem││────────┐       ┌───────┐       ┌───────┐       ┌───────┐       ┌───────┐                                                       │
    │                  ││        └───────┘       └───────┘       └───────┘       └───────┘       └───────                                                │
    │load$o$hart_to_mem││                                                                                                                                │
    │                  ││────────────────────────────────────────────────────────────────────────────────                                                │
    │                  ││────────────────────────────────────────────────────────────────────────────────                                                │
    │load$o$hart_to_mem││ 00000000                                                                                                                       │
    │                  ││────────────────────────────────────────────────────────────────────────────────                                                │
    │                  ││────────┬───────┬───────────────┬───────────────┬───────────────┬───────┬───────                                                │
    │load$o$new_rd     ││ 000000.│000000.│00000003       │00000002       │00000001       │000000.│000000.                                                │
    │                  ││────────┴───────┴───────────────┴───────────────┴───────────────┴───────┴───────                                                │
    │                  ││────────────────────────────────────────────────────────────────────────────────                                                │
    │load$unaligned_bit││ 0                                                                                                                              │
    │                  ││────────────────────────────────────────────────────────────────────────────────                                                │
    │                  ││────────────────────────────────────────────────────────────────────────────────                                                │
    │main_memory_bram  ││ 00000000                                                                                                                       │
    │                  ││────────────────────────────────────────────────────────────────────────────────                                                │
    │                  ││────────────────────────────────────────────────────────────────┬───────┬───────                                                │
    └──────────────────┘└────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┘
    384c4eceb00db3f4450a1fc47cb3e13a |}]
;;

(* TODO: Sign extension tests *)
