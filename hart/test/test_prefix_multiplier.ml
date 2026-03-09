open Core
open Hardcaml
open Hardcaml_test_harness
open Hardcaml_risc_v_hart
open Bits

module Mul = Prefix_multiplier.Make (struct
    let width = 64
  end)

module Harness = Cyclesim_harness.Make (Mul.I) (Mul.O)

let debug = true

let create_sim f =
  Harness.run
    ~waves_config:(Waves_config.to_home_subdirectory_when debug)
    ~create:Mul.hierarchical
    (fun ~inputs:_ ~outputs:_ sim -> f sim)
;;

let print (outputs : _ Mul.O.t) =
  let open Bits in
  print_s [%message "" ~rd:(!(outputs.value) |> to_int_trunc : int)]
;;

let clear sim =
  let inputs : _ Mul.I.t = Cyclesim.inputs sim in
  inputs.clock.clear := vdd;
  Cyclesim.cycle sim;
  inputs.clock.clear := gnd
;;

let test ~lhs ~rhs sim =
  let inputs : _ Mul.I.t = Cyclesim.inputs sim in
  inputs.valid := vdd;
  inputs.lhs := of_unsigned_int ~width:64 lhs;
  inputs.rhs := of_unsigned_int ~width:64 rhs;
  let outputs : _ Mul.O.t = Cyclesim.outputs sim in
  Cyclesim.cycle sim;
  inputs.valid := gnd;
  let rec poll () =
    if Bits.to_bool !(outputs.valid)
    then ()
    else (
      Cyclesim.cycle sim;
      poll ())
  in
  poll ();
  print outputs
;;

let%expect_test "op_tests" =
  create_sim (fun sim ->
    clear sim;
    test ~lhs:5 ~rhs:3 sim;
    test ~lhs:4 ~rhs:4 sim;
    test ~lhs:9 ~rhs:10 sim;
    test ~lhs:7 ~rhs:6 sim;
    [%expect
      {|
      (rd 15)
      (rd 16)
      (rd 90)
      (rd 42)
      |}]);
  [%expect {| Saved waves to /home/ubuntu/waves//_op_tests.hardcamlwaveform |}]
;;
