open Core
open Hardcaml
open Hardcaml_risc_v_hart
open Hardcaml_test_harness
open Bits
module Registers = Registers.Make (Example_hart)
module Decoded_instruction = Decoded_instruction.Make (Example_hart) (Registers)
module Csr = Csr.Make (Example_hart) (Registers) (Decoded_instruction)
module Harness = Cyclesim_harness.Make (Csr.I) (Csr.O)

let create_sim f =
  Harness.run ~create:Csr.hierarchical (fun ~inputs:_ ~outputs:_ sim -> f sim)
;;

let print (outputs : _ Csr.O.t) =
  print_s [%message "" ~_:(Csr.O.map ~f:to_int_trunc outputs : int Csr.O.t)]
;;

let read_register ~address sim =
  let inputs : _ Csr.I.t = Cyclesim.inputs sim in
  inputs.valid := vdd;
  inputs.instruction.opcode.packed
  := (Decoded_opcode.construct_onehot_bits ~f:(function
        | Decoded_opcode.System -> vdd
        | _ -> gnd))
       .packed;
  inputs.instruction.is_csr := vdd;
  inputs.instruction.funct3
  := of_unsigned_int ~width:3 (Funct3.System.to_int Funct3.System.Csrrs);
  inputs.instruction.csr := of_unsigned_int ~width:12 address;
  Cyclesim.cycle sim;
  inputs.valid := gnd
;;

let%expect_test "read registers test" =
  create_sim (fun sim ->
    let outputs : _ Csr.O.t = Cyclesim.outputs sim in
    Sequence.range 0 100 |> Sequence.iter ~f:(fun _i -> Cyclesim.cycle sim);
    print_s [%message "Cycle"];
    read_register ~address:0xC00 sim;
    print (Csr.O.map ~f:(fun t -> !t) outputs);
    read_register ~address:0xC80 sim;
    print (Csr.O.map ~f:(fun t -> !t) outputs);
    read_register ~address:0xC00 sim;
    print (Csr.O.map ~f:(fun t -> !t) outputs);
    read_register ~address:0xC80 sim;
    print (Csr.O.map ~f:(fun t -> !t) outputs);
    print_s [%message "Time"];
    read_register ~address:0xC01 sim;
    print (Csr.O.map ~f:(fun t -> !t) outputs);
    read_register ~address:0xC81 sim;
    print (Csr.O.map ~f:(fun t -> !t) outputs);
    read_register ~address:0xC01 sim;
    print (Csr.O.map ~f:(fun t -> !t) outputs);
    read_register ~address:0xC81 sim;
    print (Csr.O.map ~f:(fun t -> !t) outputs);
    print_s [%message "Instret"];
    let inputs : _ Csr.I.t = Cyclesim.inputs sim in
    inputs.instret := vdd;
    Sequence.range 0 25 |> Sequence.iter ~f:(fun _i -> Cyclesim.cycle sim);
    read_register ~address:0xC02 sim;
    print (Csr.O.map ~f:(fun t -> !t) outputs);
    read_register ~address:0xC82 sim;
    print (Csr.O.map ~f:(fun t -> !t) outputs);
    read_register ~address:0xC02 sim;
    print (Csr.O.map ~f:(fun t -> !t) outputs);
    read_register ~address:0xC82 sim;
    print (Csr.O.map ~f:(fun t -> !t) outputs));
  [%expect
    {|
    Cycle
    ((valid 1) (value 100))
    ((valid 1) (value 0))
    ((valid 1) (value 102))
    ((valid 1) (value 0))
    Time
    ((valid 1) (value 2080))
    ((valid 1) (value 0))
    ((valid 1) (value 2120))
    ((valid 1) (value 0))
    Instret
    ((valid 1) (value 25))
    ((valid 1) (value 0))
    ((valid 1) (value 27))
    ((valid 1) (value 0))
    |}]
;;
