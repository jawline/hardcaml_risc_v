open! Core
open Hardcaml
open Hardcaml_waveterm
open Risc_v_hardcaml
open! Bits

module Hart_config = struct
  let address_width = Address_width.RV32
  let register_width = Register_width.B32
  let num_registers = 32
end

module Memory_controller = Memory_controller.Make (struct
    let num_bytes = 4096
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
      ; source : 'a [@bits 32]
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

  let create (scope : Scope.t) ({ I.clock; clear; enable; funct3; source } : _ I.t) =
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
        ; source
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

let test ~source ~funct3 sim =
  let inputs : _ Test_machine.I.t = Cyclesim.inputs sim in
  let outputs : _ Test_machine.O.t = Cyclesim.outputs sim in
  inputs.enable := of_int ~width:1 1;
  inputs.source := of_int ~width:32 source;
  inputs.funct3 := of_int ~width:3 funct3;
  let rec loop_until_finished max =
    if max = 0 then raise_s [%message "BUG: Timed out"];
    Cyclesim.cycle sim;
    if Bits.to_bool !(outputs.finished) then () else loop_until_finished (max - 1)
  in
  loop_until_finished 50;
  print_s [%message (outputs : Bits.t ref Test_machine.O.t)]
;;

let%expect_test "lw" =
  let sim = create_sim () in
  let waveform, sim = Waveform.create sim in
  (try test ~source:0 ~funct3:(Funct3.Load.to_int Funct3.Load.Lw) sim with
  | _ ->
    print_s [%message "BUG: Timed out or exception"]);
    Waveform.expect
      ~serialize_to:"/tmp/test_load"
      ~display_width:150
      ~display_height:100
      waveform;
  [%expect {|
    (outputs
     ((new_rd 00000000000000000000000000000000) (error 0) (finished 1)
      (controller_to_hart
       ((valid 1)
        (data ((error 0) (read_data 00000000000000000000000000000000)))))
      (hart_to_memory_controller ((ready 0)))))
    ┌Signals───────────┐┌Waves───────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┐
    │clock             ││┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   │
    │                  ││    └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───│
    │clear             ││                                                                                                                                │
    │                  ││────────                                                                                                                        │
    │enable            ││────────                                                                                                                        │
    │                  ││                                                                                                                                │
    │                  ││────────                                                                                                                        │
    │funct3            ││ 2                                                                                                                              │
    │                  ││────────                                                                                                                        │
    │                  ││────────                                                                                                                        │
    │source            ││ 000000.                                                                                                                        │
    │                  ││────────                                                                                                                        │
    │controller_to_hart││                                                                                                                                │
    │                  ││────────                                                                                                                        │
    │                  ││────────                                                                                                                        │
    │controller_to_hart││ 000000.                                                                                                                        │
    │                  ││────────                                                                                                                        │
    │controller_to_hart││────────                                                                                                                        │
    │                  ││                                                                                                                                │
    │error             ││                                                                                                                                │
    │                  ││────────                                                                                                                        │
    │finished          ││────────                                                                                                                        │
    │                  ││                                                                                                                                │
    │hart_to_controller││                                                                                                                                │
    │                  ││────────                                                                                                                        │
    │                  ││────────                                                                                                                        │
    │new_rd            ││ 000000.                                                                                                                        │
    │                  ││────────                                                                                                                        │
    │                  ││────────                                                                                                                        │
    │load$aligned_addre││ FFFFFF.                                                                                                                        │
    │                  ││────────                                                                                                                        │
    │load$current_state││                                                                                                                                │
    │                  ││────────                                                                                                                        │
    │load$funct3_is_err││                                                                                                                                │
    │                  ││────────                                                                                                                        │
    │load$i$clear      ││                                                                                                                                │
    │                  ││────────                                                                                                                        │
    │load$i$clock      ││                                                                                                                                │
    │                  ││────────                                                                                                                        │
    │load$i$enable     ││────────                                                                                                                        │
    │                  ││                                                                                                                                │
    │                  ││────────                                                                                                                        │
    │load$i$funct3     ││ 2                                                                                                                              │
    │                  ││────────                                                                                                                        │
    │load$i$hart_to_mem││                                                                                                                                │
    │                  ││────────                                                                                                                        │
    │load$i$memory_cont││                                                                                                                                │
    │                  ││────────                                                                                                                        │
    │                  ││────────                                                                                                                        │
    │load$i$memory_cont││ 000000.                                                                                                                        │
    │                  ││────────                                                                                                                        │
    │load$i$memory_cont││────────                                                                                                                        │
    │                  ││                                                                                                                                │
    │                  ││────────                                                                                                                        │
    │load$i$source     ││ 000000.                                                                                                                        │
    │                  ││────────                                                                                                                        │
    │load$inputs_are_er││                                                                                                                                │
    │                  ││────────                                                                                                                        │
    │load$is_unaligned ││                                                                                                                                │
    │                  ││────────                                                                                                                        │
    │load$o$error      ││                                                                                                                                │
    │                  ││────────                                                                                                                        │
    │load$o$finished   ││────────                                                                                                                        │
    │                  ││                                                                                                                                │
    │                  ││────────                                                                                                                        │
    │load$o$hart_to_mem││ 000000.                                                                                                                        │
    │                  ││────────                                                                                                                        │
    │load$o$hart_to_mem││────────                                                                                                                        │
    │                  ││                                                                                                                                │
    │load$o$hart_to_mem││                                                                                                                                │
    │                  ││────────                                                                                                                        │
    │                  ││────────                                                                                                                        │
    │load$o$hart_to_mem││ 000000.                                                                                                                        │
    │                  ││────────                                                                                                                        │
    │                  ││────────                                                                                                                        │
    │load$o$new_rd     ││ 000000.                                                                                                                        │
    │                  ││────────                                                                                                                        │
    │                  ││────────                                                                                                                        │
    │load$unaligned_bit││ 0                                                                                                                              │
    │                  ││────────                                                                                                                        │
    │                  ││────────                                                                                                                        │
    │memory_controller$││ 000000.                                                                                                                        │
    │                  ││────────                                                                                                                        │
    │memory_controller$││────────                                                                                                                        │
    │                  ││                                                                                                                                │
    │memory_controller$││                                                                                                                                │
    │                  ││────────                                                                                                                        │
    │                  ││────────                                                                                                                        │
    │memory_controller$││ 000000.                                                                                                                        │
    │                  ││────────                                                                                                                        │
    │memory_controller$││                                                                                                                                │
    │                  ││────────                                                                                                                        │
    │memory_controller$││                                                                                                                                │
    │                  ││────────                                                                                                                        │
    │memory_controller$││                                                                                                                                │
    │                  ││────────                                                                                                                        │
    │memory_controller$││────────                                                                                                                        │
    │                  ││                                                                                                                                │
    └──────────────────┘└────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┘
    17d20d261a35916cec139c9d90a88acb |}]
;;
