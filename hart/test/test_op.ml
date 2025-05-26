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

let create_sim f = Harness.run ~create:Op.create (fun ~inputs:_ ~outputs:_ sim -> f sim)

let print (outputs : _ Op.O.t) =
  let open Bits in
  print_s [%message "" ~rd:(!(outputs.rd) |> to_int_trunc : int)]
;;

let test ~lhs ~rhs ~funct3 ~funct7 sim =
  let inputs : _ Op.I.t = Cyclesim.inputs sim in
  inputs.lhs := of_unsigned_int ~width:32 lhs;
  inputs.rhs := of_unsigned_int ~width:32 rhs;
  inputs.funct3.packed
  := (Funct3.Op.Onehot.construct_onehot_bits ~f:(fun t ->
        Funct3.Op.equal t funct3 |> Bits.of_bool))
       .packed;
  inputs.subtract_instead_of_add := of_bool funct7;
  inputs.arithmetic_shift := of_bool funct7;
  Cyclesim.cycle sim
;;

let simple_test ~funct3 sim =
  let outputs = Cyclesim.outputs sim in
  printf "Funct 7 = 0\n";
  test ~lhs:17 ~rhs:3 ~funct3 ~funct7:false sim;
  print outputs;
  printf "Funct 7 = 1\n";
  test ~lhs:17 ~rhs:3 ~funct3 ~funct7:true sim;
  print outputs
;;

let%expect_test "branch tests" =
  create_sim (fun sim ->
    simple_test ~funct3:Add_or_sub sim;
    [%expect
      {|
      Funct 7 = 0
      (rd 20)
      Funct 7 = 1
      (rd 14)
      |}];
    simple_test ~funct3:Sll sim;
    [%expect
      {|
      Funct 7 = 0
      (rd 136)
      Funct 7 = 1
      (rd 136)
      |}];
    simple_test ~funct3:Slt sim;
    [%expect
      {|
      Funct 7 = 0
      (rd 0)
      Funct 7 = 1
      (rd 0)
      |}];
    simple_test ~funct3:Xor sim;
    [%expect
      {|
      Funct 7 = 0
      (rd 18)
      Funct 7 = 1
      (rd 18)
      |}];
    simple_test ~funct3:Sltu sim;
    [%expect
      {|
      Funct 7 = 0
      (rd 0)
      Funct 7 = 1
      (rd 0)
      |}];
    simple_test ~funct3:Or sim;
    [%expect
      {|
      Funct 7 = 0
      (rd 19)
      Funct 7 = 1
      (rd 19)
      |}];
    simple_test ~funct3:And sim;
    [%expect
      {|
      Funct 7 = 0
      (rd 1)
      Funct 7 = 1
      (rd 1)
      |}];
    simple_test ~funct3:Srl_or_sra sim;
    [%expect
      {|
      Funct 7 = 0
      (rd 2)
      Funct 7 = 1
      (rd 2)
      |}])
;;
