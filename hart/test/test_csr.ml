open Core
open Hardcaml
open Hardcaml_risc_v_hart
open Bits

module Hart_config = struct
  let register_width = Register_width.B32
  let num_registers = 32
  let design_frequency = 50_000_000
end

module Registers = Registers.Make (Hart_config)
module Decoded_instruction = Decoded_instruction.Make (Hart_config) (Registers)
module Csr = Csr.Make (Hart_config) (Registers) (Decoded_instruction)

let create_sim () =
  let module Sim = Cyclesim.With_interface (Csr.I) (Csr.O) in
  Sim.create (Csr.hierarchical (Scope.create ~flatten_design:true ()))
;;

let print (outputs : _ Csr.O.t) =
  print_s [%message "" (Csr.O.map ~f:Bits.to_int outputs : int Csr.O.t)]
;;

let read_register ~address sim =
  let inputs : _ Csr.I.t = Cyclesim.inputs sim in
  inputs.valid := vdd;
  inputs.instruction.funct3 := of_int ~width:3 (Funct3.System.to_int Funct3.System.Csrrs);
  inputs.instruction.csr := of_int ~width:12 address;
  Cyclesim.cycle sim;
  inputs.valid := gnd
;;

let%expect_test "read registers test" =
  let sim = create_sim () in
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
  inputs.instret := Bits.vdd;
  Sequence.range 0 25 |> Sequence.iter ~f:(fun _i -> Cyclesim.cycle sim);
  read_register ~address:0xC02 sim;
  print (Csr.O.map ~f:(fun t -> !t) outputs);
  read_register ~address:0xC82 sim;
  print (Csr.O.map ~f:(fun t -> !t) outputs);
  read_register ~address:0xC02 sim;
  print (Csr.O.map ~f:(fun t -> !t) outputs);
  read_register ~address:0xC82 sim;
  print (Csr.O.map ~f:(fun t -> !t) outputs);
  [%expect
    {|
    Cycle
    ("Csr.O.map ~f:Bits.to_int outputs" ((valid 1) (value 100)))
    ("Csr.O.map ~f:Bits.to_int outputs" ((valid 1) (value 0)))
    ("Csr.O.map ~f:Bits.to_int outputs" ((valid 1) (value 102)))
    ("Csr.O.map ~f:Bits.to_int outputs" ((valid 1) (value 0)))
    Time
    ("Csr.O.map ~f:Bits.to_int outputs" ((valid 1) (value 2080)))
    ("Csr.O.map ~f:Bits.to_int outputs" ((valid 1) (value 0)))
    ("Csr.O.map ~f:Bits.to_int outputs" ((valid 1) (value 2120)))
    ("Csr.O.map ~f:Bits.to_int outputs" ((valid 1) (value 0)))
    Instret
    ("Csr.O.map ~f:Bits.to_int outputs" ((valid 1) (value 25)))
    ("Csr.O.map ~f:Bits.to_int outputs" ((valid 1) (value 0)))
    ("Csr.O.map ~f:Bits.to_int outputs" ((valid 1) (value 27)))
    ("Csr.O.map ~f:Bits.to_int outputs" ((valid 1) (value 0)))
    |}]
;;
