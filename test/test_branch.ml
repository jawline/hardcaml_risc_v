open Core
open Hardcaml
open Risc_v_hardcaml
open Bits

module Hart_config = struct
  let address_width = Address_width.RV32
  let register_width = Register_width.B32
  let num_registers = 32
end

module Branch = Branch.Make (Hart_config)

let create_sim () =
  let module Sim = Cyclesim.With_interface (Branch.I) (Branch.O) in
  Sim.create (Branch.create (Scope.create ()))
;;

let print (outputs : _ Branch.O.t) =
  let open Bits in
  print_s
    [%message
      ""
        ~pc:(!(outputs.new_pc) |> to_int : int)
        ~error:(!(outputs.error) |> to_bool : bool)]
;;

let test ~pc ~lhs ~rhs ~funct3 ~b_immediate sim =
  let inputs : _ Branch.I.t = Cyclesim.inputs sim in
  inputs.pc := of_int ~width:32 pc;
  inputs.lhs := of_int ~width:32 lhs;
  inputs.rhs := of_int ~width:32 rhs;
  inputs.funct3 := of_int ~width:3 funct3;
  inputs.b_immediate := of_int ~width:32 b_immediate;
  Cyclesim.cycle sim
;;

let%expect_test "branch tests" =
  let sim = create_sim () in
  let outputs = Cyclesim.outputs sim in
  test ~pc:500 ~lhs:500 ~rhs:55 ~funct3:0 ~b_immediate:1500 sim;
  print outputs;
  [%expect {| ((pc 504) (error false)) |}];
  test ~pc:500 ~lhs:500 ~rhs:500 ~funct3:0 ~b_immediate:1500 sim;
  print outputs;
  [%expect {| ((pc 2000) (error false)) |}];
  test ~pc:500 ~lhs:500 ~rhs:55 ~funct3:1 ~b_immediate:1500 sim;
  print outputs;
  [%expect {| ((pc 504) (error false)) |}];
  test ~pc:500 ~lhs:500 ~rhs:500 ~funct3:1 ~b_immediate:1500 sim;
  print outputs;
  [%expect {| ((pc 2000) (error false)) |}]
;;

(* TODO: Test the other branches, but this shows the sketch works. *)
