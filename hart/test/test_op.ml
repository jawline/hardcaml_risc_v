open Core
open Hardcaml
open Hardcaml_test_harness
open Hardcaml_risc_v_hart
open Bits

module Hart_config = struct
  let register_width = Register_width.B32
  let num_registers = 32
  let design_frequency = 50_000_000_000
end

module Op = Op.Make (Hart_config)
module Harness = Cyclesim_harness.Make (Op.I) (Op.O)

let create_sim f =
  Harness.run ~create:Op.hierarchical (fun ~inputs:_ ~outputs:_ sim -> f sim)
;;

let print (outputs : _ Op.O.t) =
  let open Bits in
  print_s [%message "" ~rd:(!(outputs.rd) |> to_int_trunc : int)]
;;

let test ~lhs ~rhs ~op sim =
  let inputs : _ Op.I.t = Cyclesim.inputs sim in
  inputs.lhs := of_unsigned_int ~width:32 lhs;
  inputs.rhs := of_unsigned_int ~width:32 rhs;
  inputs.op.packed
  := (Alu_operation.Onehot.construct_onehot_bits ~f:(fun t ->
        Alu_operation.equal t op |> Bits.of_bool))
       .packed;
  Cyclesim.cycle sim
;;

let simple_test ~op sim =
  let outputs = Cyclesim.outputs sim in
  printf "Funct 7 = 0\n";
  test ~lhs:17 ~rhs:3 ~op sim;
  print outputs
;;

let%expect_test "branch tests" =
  create_sim (fun sim ->
    simple_test ~op:Add sim;
    [%expect
      {|
      Funct 7 = 0
      (rd 20)
      |}];
    simple_test ~op:Sub sim;
    [%expect
      {|
      Funct 7 = 0
      (rd 14)
      |}];
    simple_test ~op:Sll sim;
    [%expect
      {|
      Funct 7 = 0
      (rd 136)
      |}];
    simple_test ~op:Slt sim;
    [%expect
      {|
      Funct 7 = 0
      (rd 0)
      |}];
    simple_test ~op:Xor sim;
    [%expect
      {|
      Funct 7 = 0
      (rd 18)
      |}];
    simple_test ~op:Sltu sim;
    [%expect
      {|
      Funct 7 = 0
      (rd 0)
      |}];
    simple_test ~op:Or sim;
    [%expect
      {|
      Funct 7 = 0
      (rd 19)
      |}];
    simple_test ~op:And sim;
    [%expect
      {|
      Funct 7 = 0
      (rd 1)
      |}];
    simple_test ~op:Srl sim;
    [%expect
      {|
      Funct 7 = 0
      (rd 2)
      |}];
    simple_test ~op:Sra sim;
    [%expect
      {|
      Funct 7 = 0
      (rd 2)
      |}])
;;
