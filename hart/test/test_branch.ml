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

module Branch = Branch.Make (Hart_config)
module Harness = Cyclesim_harness.Make (Branch.I) (Branch.O)

let create_sim f =
  Harness.run ~create:Branch.create (fun ~inputs:_ ~outputs:_ sim -> f sim)
;;

let print (outputs : _ Branch.O.t) =
  let open Bits in
  print_s [%message "" ~pc:(!(outputs.new_pc) |> to_unsigned_int : int)]
;;

let test ~pc ~lhs ~rhs ~funct3 ~b_immediate sim =
  let inputs : _ Branch.I.t = Cyclesim.inputs sim in
  inputs.pc := of_unsigned_int ~width:32 pc;
  inputs.lhs := of_unsigned_int ~width:32 lhs;
  inputs.rhs := of_unsigned_int ~width:32 rhs;
  inputs.funct3.packed
  := (Funct3.Branch.Onehot.construct_onehot_bits ~f:(fun t ->
        Funct3.Branch.equal t funct3 |> Bits.of_bool))
       .packed;
  inputs.branch_offset := of_unsigned_int ~width:32 b_immediate;
  Cyclesim.cycle sim
;;

let simple_test ~funct3 sim =
  let outputs = Cyclesim.outputs sim in
  test ~pc:500 ~lhs:500 ~rhs:55 ~funct3 ~b_immediate:1500 sim;
  print outputs;
  test ~pc:500 ~lhs:55 ~rhs:500 ~funct3 ~b_immediate:1500 sim;
  print outputs;
  test ~pc:500 ~lhs:500 ~rhs:500 ~funct3 ~b_immediate:1500 sim;
  print outputs
;;

let%expect_test "branch tests" =
  create_sim (fun sim ->
    simple_test ~funct3:Beq sim;
    [%expect
      {|
      (pc 504)
      (pc 504)
      (pc 2000)
      |}];
    simple_test ~funct3:Bne sim;
    [%expect
      {|
      (pc 2000)
      (pc 2000)
      (pc 504)
      |}];
    simple_test ~funct3:Blt sim;
    [%expect
      {|
      (pc 504)
      (pc 2000)
      (pc 504)
      |}];
    simple_test ~funct3:Bge sim;
    [%expect
      {|
      (pc 2000)
      (pc 504)
      (pc 2000)
      |}];
    (* TODO: Add explicit tests for unsigned registers. *)
    simple_test ~funct3:Bltu sim;
    [%expect
      {|
      (pc 504)
      (pc 2000)
      (pc 504)
      |}];
    simple_test ~funct3:Bgeu sim;
    [%expect
      {|
      (pc 2000)
      (pc 504)
      (pc 2000)
      |}])
;;
