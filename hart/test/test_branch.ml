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
  print_s
    [%message
      ""
        ~pc:(!(outputs.new_pc) |> to_unsigned_int : int)
        ~error:(!(outputs.error) |> to_bool : bool)]
;;

let test ~pc ~lhs ~rhs ~funct3 ~b_immediate sim =
  let inputs : _ Branch.I.t = Cyclesim.inputs sim in
  inputs.pc := of_unsigned_int ~width:32 pc;
  inputs.lhs := of_unsigned_int ~width:32 lhs;
  inputs.rhs := of_unsigned_int ~width:32 rhs;
  inputs.funct3 := of_unsigned_int ~width:3 funct3;
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
    simple_test ~funct3:(Funct3.Branch.to_int Beq) sim;
    [%expect
      {|
      ((pc 504) (error false))
      ((pc 504) (error false))
      ((pc 2000) (error false)) |}];
    simple_test ~funct3:(Funct3.Branch.to_int Bne) sim;
    [%expect
      {|
    ((pc 2000) (error false))
    ((pc 2000) (error false))
    ((pc 504) (error false)) |}];
    simple_test ~funct3:(Funct3.Branch.to_int Blt) sim;
    [%expect
      {|
    ((pc 504) (error false))
    ((pc 2000) (error false))
    ((pc 504) (error false)) |}];
    simple_test ~funct3:(Funct3.Branch.to_int Bge) sim;
    [%expect
      {|
    ((pc 2000) (error false))
    ((pc 504) (error false))
    ((pc 2000) (error false)) |}];
    (* TODO: Add explicit tests for unsigned registers. *)
    simple_test ~funct3:(Funct3.Branch.to_int Bltu) sim;
    [%expect
      {|
    ((pc 504) (error false))
    ((pc 2000) (error false))
    ((pc 504) (error false)) |}];
    simple_test ~funct3:(Funct3.Branch.to_int Bgeu) sim;
    [%expect
      {|
    ((pc 2000) (error false))
    ((pc 504) (error false))
    ((pc 2000) (error false)) |}])
;;
