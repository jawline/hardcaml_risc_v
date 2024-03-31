open! Core
open Hardcaml
open Hardcaml_waveterm
open Risc_v_hardcaml
open! Bits

module Cpu =
  Cpu.Make
    (struct
      let address_width = Address_width.RV32
      let register_width = Register_width.B32
      let num_registers = 32
    end)
    (struct
      let num_bytes = 32
    end)
    (struct
      let num_harts = 1
    end)

let create_sim () =
  let module Sim = Cyclesim.With_interface (Cpu.I) (Cpu.O) in
  Sim.create
    ~config:Cyclesim.Config.trace_all
    (Cpu.create
       (Scope.create ~auto_label_hierarchical_ports:true ~flatten_design:true ()))
;;

let print_ram sim =
  let ram = Cyclesim.lookup_mem sim "main_memory_bram" |> Option.value_exn in
  Array.map ~f:(fun mut -> Bits.Mutable.to_bits mut |> Bits.to_int) ram
  |> Array.iter ~f:(fun v -> printf "%02x " v);
  printf "\n"
;;

let test ?(clear_registers = true) ~instructions sim =
  (* Initialize the main memory to some known values for testing. *)
  let initial_ram = Cyclesim.lookup_mem sim "main_memory_bram" |> Option.value_exn in
  Array.iteri
    ~f:(fun index tgt ->
      match List.nth instructions index with
      | Some instruction -> Bits.Mutable.copy_bits ~src:instruction ~dst:tgt
      | None -> ())
    initial_ram;
  let inputs : _ Cpu.I.t = Cyclesim.inputs sim in
  let _outputs_before : _ Cpu.O.t = Cyclesim.outputs ~clock_edge:Side.Before sim in
  let outputs : _ Cpu.O.t = Cyclesim.outputs sim in
  if clear_registers
  then (
    inputs.clear := Bits.one 1;
    Cyclesim.cycle sim;
    inputs.clear := Bits.zero 1);
  let rec loop_for cycles =
    if cycles = 0
    then ()
    else (
      Cyclesim.cycle sim;
      loop_for (cycles - 1))
  in
  loop_for 20;
  let outputs = Cpu.O.map ~f:(fun t -> Bits.to_int !t) outputs in
  print_s [%message (outputs : int Cpu.O.t)];
  print_ram sim
;;

let assemble_i_type ~opcode ~funct3 ~rs1 ~rd ~immediate =
  concat_msb [ immediate; rs1; funct3; rd; opcode ]
;;

let op_imm ~funct3 ~rs1 ~rd ~immediate =
  assemble_i_type
    ~opcode:(Bits.of_int ~width:7 Opcodes.op_imm)
    ~funct3:(Bits.of_int ~width:3 (Funct3.Op.to_int funct3))
    ~rs1:(Bits.of_int ~width:5 rs1)
    ~rd:(Bits.of_int ~width:5 rd)
    ~immediate:(Bits.of_int ~width:12 immediate)
;;

let%expect_test "add" =
  let sim = create_sim () in
  let waveform, sim = Waveform.create sim in
  (* test
     ~instructions:[ op_imm ~funct3:Funct3.Op.Add_or_sub ~rs1:0 ~rd:1 ~immediate:550 ]
     sim; *)
  [%expect {| |}];
  test
    ~instructions:
      [ op_imm ~funct3:Funct3.Op.Xor ~rs1:0 ~rd:1 ~immediate:0b0101
      ; op_imm ~funct3:Funct3.Op.Xor ~rs1:1 ~rd:1 ~immediate:0b1010
      ]
    sim;
  [%expect
    {|
     (outputs
      ((registers
        (((pc 8)
          (general
           (0 15 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))))))
     504093 a0c093 00 00 00 00 00 00 |}];
  Waveform.expect
    ~serialize_to:"/tmp/test_cpu"
    ~display_width:150
    ~display_height:100
    waveform;
  [%expect
    {|
    ┌Signals───────────┐┌Waves───────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┐
    │clock             ││┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   │
    │                  ││    └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───│
    │clear             ││────────┐                                                                                                                       │
    │                  ││        └───────────────────────────────────────────────────────────────────────────────────────────────────────────────────────│
    │                  ││────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────│
    │general00         ││ 00000000                                                                                                                       │
    │                  ││────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────│
    │                  ││────────────────────────────────────────────────────────────────────────────────┬───────────────────────────────────────────────│
    │general10         ││ 00000000                                                                       │00000005                                       │
    │                  ││────────────────────────────────────────────────────────────────────────────────┴───────────────────────────────────────────────│
    │                  ││────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────│
    │general100        ││ 00000000                                                                                                                       │
    │                  ││────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────│
    │                  ││────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────│
    │general110        ││ 00000000                                                                                                                       │
    │                  ││────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────│
    │                  ││────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────│
    │general120        ││ 00000000                                                                                                                       │
    │                  ││────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────│
    │                  ││────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────│
    │general130        ││ 00000000                                                                                                                       │
    │                  ││────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────│
    │                  ││────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────│
    │general140        ││ 00000000                                                                                                                       │
    │                  ││────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────│
    │                  ││────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────│
    │general150        ││ 00000000                                                                                                                       │
    │                  ││────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────│
    │                  ││────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────│
    │general160        ││ 00000000                                                                                                                       │
    │                  ││────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────│
    │                  ││────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────│
    │general170        ││ 00000000                                                                                                                       │
    │                  ││────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────│
    │                  ││────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────│
    │general180        ││ 00000000                                                                                                                       │
    │                  ││────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────│
    │                  ││────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────│
    │general190        ││ 00000000                                                                                                                       │
    │                  ││────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────│
    │                  ││────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────│
    │general20         ││ 00000000                                                                                                                       │
    │                  ││────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────│
    │                  ││────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────│
    │general200        ││ 00000000                                                                                                                       │
    │                  ││────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────│
    │                  ││────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────│
    │general210        ││ 00000000                                                                                                                       │
    │                  ││────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────│
    │                  ││────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────│
    │general220        ││ 00000000                                                                                                                       │
    │                  ││────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────│
    │                  ││────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────│
    │general230        ││ 00000000                                                                                                                       │
    │                  ││────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────│
    │                  ││────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────│
    │general240        ││ 00000000                                                                                                                       │
    │                  ││────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────│
    │                  ││────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────│
    │general250        ││ 00000000                                                                                                                       │
    │                  ││────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────│
    │                  ││────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────│
    │general260        ││ 00000000                                                                                                                       │
    │                  ││────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────│
    │                  ││────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────│
    │general270        ││ 00000000                                                                                                                       │
    │                  ││────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────│
    │                  ││────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────│
    │general280        ││ 00000000                                                                                                                       │
    │                  ││────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────│
    │                  ││────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────│
    │general290        ││ 00000000                                                                                                                       │
    │                  ││────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────│
    │                  ││────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────│
    │general30         ││ 00000000                                                                                                                       │
    │                  ││────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────│
    │                  ││────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────│
    │general300        ││ 00000000                                                                                                                       │
    │                  ││────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────│
    │                  ││────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────│
    │general310        ││ 00000000                                                                                                                       │
    │                  ││────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────│
    │                  ││────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────│
    │general40         ││ 00000000                                                                                                                       │
    │                  ││────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────│
    │                  ││────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────│
    │general50         ││ 00000000                                                                                                                       │
    │                  ││────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────│
    │                  ││────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────│
    │general60         ││ 00000000                                                                                                                       │
    │                  ││────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────│
    │                  ││────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────│
    │general70         ││ 00000000                                                                                                                       │
    │                  ││────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────│
    │                  ││────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────│
    │general80         ││ 00000000                                                                                                                       │
    │                  ││────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────│
    │                  ││────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────│
    └──────────────────┘└────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┘
    5773f3efee8b7905ae70b0a715834ae0 |}]
;;
