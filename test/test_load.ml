open! Core
open Hardcaml
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
        ~instance:"controller"
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
    { O.new_rd = load.new_rd; O.error = load.error; O.finished = load.finished }
  ;;
end

let create_sim () =
  let module Sim = Cyclesim.With_interface (Test_machine.I) (Test_machine.O) in
  Sim.create (Test_machine.create (Scope.create ~flatten_design:true ()))
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

    print_s [%message (outputs : Bits.t ref Test_machine.O.t)];
    if Bits.to_bool !(outputs.finished) then () else loop_until_finished (max - 1)
  in
  loop_until_finished 10_000;
  print_s [%message (outputs : Bits.t ref Test_machine.O.t)]
;;

let%expect_test "lw" =
  let sim = create_sim () in
  test ~source:0 ~funct3:(Funct3.Load.to_int Funct3.Load.Lw) sim;
  [%expect.unreachable]
[@@expect.uncaught_exn
  {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  "BUG: Timed out"
  Raised at Base__Error.raise in file "src/error.ml" (inlined), line 9, characters 14-30
  Called from Base__Error.raise_s in file "src/error.ml", line 10, characters 19-40
  Called from Hardcaml_risc_v_test__Test_load.test.loop_until_finished in file "test/test_load.ml", line 97, characters 20-55
  Called from Hardcaml_risc_v_test__Test_load.test in file "test/test_load.ml", line 101, characters 2-28
  Called from Hardcaml_risc_v_test__Test_load.(fun) in file "test/test_load.ml", line 107, characters 2-64
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 234, characters 12-19 |}]
;;
