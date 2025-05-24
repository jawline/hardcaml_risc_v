open Core
open Hardcaml
open Hardcaml_memory_controller
open Hardcaml_test_harness
open Hardcaml_risc_v_hart
open! Bits

module Hart_config = struct
  let register_width = Register_width.B32
  let num_registers = 32
  let design_frequency = 50_000
end

module Memory_controller = Memory_controller.Make (struct
    let capacity_in_bytes = 128
    let num_read_channels = 1
    let num_write_channels = 1
    let address_width = 32
    let data_bus_width = 32
  end)

module Registers = Registers.Make (Hart_config)
module Decoded_instruction = Decoded_instruction.Make (Hart_config) (Registers)
module Transaction = Transaction.Make (Hart_config) (Memory_controller.Memory_bus)

module Execute =
  Execute.Make (Hart_config) (Memory_controller.Memory_bus) (Registers)
    (Decoded_instruction)
    (Transaction)

module Harness = Cyclesim_harness.Make (Execute.I) (Execute.O)

let create_sim f =
  Harness.run
    ~waves_config:(Waves_config.to_home_subdirectory ())
    ~create:Execute.hierarchical
    (fun ~inputs:_ ~outputs:_ sim -> f sim)
;;

let print (outputs : Bits.t Execute.O.t) =
  print_s [%message "" ~_:(Execute.O.map ~f:Bits.to_int_trunc outputs : int Execute.O.t)]
;;

let deref (outputs : Bits.t ref Execute.O.t) = Execute.O.map ~f:(fun t -> !t) outputs

let test ~(instruction : Bits.t Decoded_instruction.t) sim =
  let inputs : _ Execute.I.t = Cyclesim.inputs sim in
  inputs.valid := vdd;
  Decoded_instruction.iter2 ~f:(fun a b -> a := b) inputs.instruction instruction;
  Cyclesim.cycle sim;
  inputs.valid := gnd;
  let outputs : Bits.t ref Execute.O.t = Cyclesim.outputs sim in
  let rec wait_for_output limit =
    if limit = 0
    then raise_s [%message "Timeout"]
    else if Bits.to_bool !(outputs.valid)
    then ()
    else wait_for_output (limit - 1)
  in
  wait_for_output 5000;
  print (deref outputs);
  Cyclesim.cycle ~n:20 sim
;;

let%expect_test "branch tests" =
  create_sim (fun sim ->
    test ~instruction:(Decoded_instruction.Of_bits.of_int 0) sim;
    [%expect.unreachable])
[@@expect.uncaught_exn
  {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)
  Timeout
  Raised at Base__Error.raise in file "src/error.ml", line 9, characters 21-37
  Called from Hardcaml_risc_v_hart_test__Test_execute.test in file "hart/test/test_execute.ml", line 60, characters 2-22
  Called from Hardcaml_risc_v_hart_test__Test_execute.(fun) in file "hart/test/test_execute.ml", line 67, characters 4-64
  Called from Base__Exn.protectx in file "src/exn.ml", line 51, characters 8-11
  Re-raised at Base__Exn.raise_with_original_backtrace in file "src/exn.ml" (inlined), line 31, characters 2-50
  Called from Base__Exn.protectx in file "src/exn.ml", line 58, characters 13-49
  Called from Ppx_expect_runtime__Test_block.Configured.dump_backtrace in file "runtime/test_block.ml", line 264, characters 10-25

  Trailing output
  ---------------
  Saved waves to /home/blake/waves//_branch_tests.hardcamlwaveform
  |}]
;;
