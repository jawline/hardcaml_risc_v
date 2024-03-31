open Core
open Hardcaml
open Risc_v_hardcaml
open Bits

module Hart_config = struct
  let address_width = Address_width.RV32
  let register_width = Register_width.B32
  let num_registers = 32
end

module Op = Op.Make (Hart_config)

let create_sim () =
  let module Sim = Cyclesim.With_interface (Op.I) (Op.O) in
  Sim.create (Op.create ~enable_subtract:true (Scope.create ()))
;;

let print (outputs : _ Op.O.t) =
  let open Bits in
  print_s
    [%message
      "" ~rd:(!(outputs.rd) |> to_int : int) ~error:(!(outputs.error) |> to_bool : bool)]
;;

let test ~lhs ~rhs ~funct3 ~funct7 sim =
  let inputs : _ Op.I.t = Cyclesim.inputs sim in
  inputs.lhs := of_int ~width:32 lhs;
  inputs.rhs := of_int ~width:32 rhs;
  inputs.funct3 := of_int ~width:3 funct3;
  inputs.funct7 := of_int ~width:7 funct7;
  Cyclesim.cycle sim
;;

let simple_test ~funct3 sim =
  let outputs = Cyclesim.outputs sim in
  printf "Funct 7 = 0\n";
  test ~lhs:17 ~rhs:3 ~funct3 ~funct7:0 sim;
  print outputs;
  printf "Funct 7 = 1\n";
  test ~lhs:17 ~rhs:3 ~funct3 ~funct7:1 sim;
  print outputs
;;

let%expect_test "branch tests" =
  let sim = create_sim () in
  simple_test ~funct3:(Funct3.Op.to_int Add_or_sub) sim;
  [%expect
    {|
      Funct 7 = 0
      ((rd 14) (error false))
      Funct 7 = 1
      ((rd 20) (error false)) |}];
  simple_test ~funct3:(Funct3.Op.to_int Sll) sim;
  [%expect
    {|
    Funct 7 = 0
    ((rd 136) (error false))
    Funct 7 = 1
    ((rd 136) (error false)) |}];
  simple_test ~funct3:(Funct3.Op.to_int Slt) sim;
  [%expect
    {|
    Funct 7 = 0
    ((rd 0) (error false))
    Funct 7 = 1
    ((rd 0) (error false)) |}];
  simple_test ~funct3:(Funct3.Op.to_int Xor) sim;
  [%expect
    {|
    Funct 7 = 0
    ((rd 18) (error false))
    Funct 7 = 1
    ((rd 18) (error false)) |}];
  simple_test ~funct3:(Funct3.Op.to_int Sltu) sim;
  [%expect
    {|
    Funct 7 = 0
    ((rd 0) (error false))
    Funct 7 = 1
    ((rd 0) (error false)) |}];
  simple_test ~funct3:(Funct3.Op.to_int Or) sim;
  [%expect
    {|
    Funct 7 = 0
    ((rd 19) (error false))
    Funct 7 = 1
    ((rd 19) (error false)) |}];
  simple_test ~funct3:(Funct3.Op.to_int And) sim;
  [%expect
    {|
    Funct 7 = 0
    ((rd 1) (error false))
    Funct 7 = 1
    ((rd 1) (error false)) |}];
  simple_test ~funct3:(Funct3.Op.to_int Srl_or_sra) sim;
  [%expect
    {|
    Funct 7 = 0
    ((rd 2) (error false))
    Funct 7 = 1
    ((rd 2) (error false)) |}]
;;
