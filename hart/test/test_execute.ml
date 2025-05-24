open Core
open Hardcaml
open Hardcaml_memory_controller
open Hardcaml_test_harness
open Hardcaml_risc_v_hart
open! Bits

module Hart_config = struct
  let register_width = Register_width.B32
  let num_registers = 32
  let design_frequency = 50_000
end

module Memory_controller = Memory_controller.Make (struct
    let capacity_in_bytes = 128
    let num_read_channels = 1
    let num_write_channels = 1
    let address_width = 32
    let data_bus_width = 32
  end)

module Registers = Registers.Make (Hart_config)
module Decoded_instruction = Decoded_instruction.Make (Hart_config) (Registers)
module Transaction = Transaction.Make (Hart_config) (Memory_controller.Memory_bus)

module Execute =
  Execute.Make (Hart_config) (Memory_controller.Memory_bus) (Registers)
    (Decoded_instruction)
    (Transaction)

module Harness = Cyclesim_harness.Make (Execute.I) (Execute.O)

let create_sim f =
  Harness.run
    ~waves_config:(Waves_config.to_home_subdirectory ())
    ~create:Execute.hierarchical
    (fun ~inputs:_ ~outputs:_ sim -> f sim)
;;

let print (outputs : Bits.t Execute.O.t) =
  print_s [%message "" ~_:(Execute.O.map ~f:Bits.to_int_trunc outputs : int Execute.O.t)]
;;

let deref (outputs : Bits.t ref Execute.O.t) = Execute.O.map ~f:(fun t -> !t) outputs

let test ~(instruction : Bits.t Decoded_instruction.t) sim =
  let inputs : _ Execute.I.t = Cyclesim.inputs sim in
  inputs.valid := vdd;
  Decoded_instruction.iter2 ~f:(fun a b -> a := b) inputs.instruction instruction;
  Cyclesim.cycle sim;
  inputs.valid := gnd;
  let outputs : Bits.t ref Execute.O.t = Cyclesim.outputs sim in
  let rec wait_for_output limit =
    if limit = 0
    then raise_s [%message "Timeout"]
    else if Bits.to_bool !(outputs.valid)
    then ()
    else wait_for_output (limit - 1)
  in
  wait_for_output 5000;
  print (deref outputs);
  Cyclesim.cycle ~n:20 sim
;;

let%expect_test "branch tests" =
  create_sim (fun sim ->
    test ~instruction:(Decoded_instruction.Of_bits.of_int 0) sim;
    [%expect
      {|
      ((valid 1)
       (registers
        ((pc 0)
         (general (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))))
       (instruction
        ((opcode ((packed 0))) (funct3 0) (funct7 0) (rs1 0) (rs2 0) (rd 0)
         (rd_value 0) (csr 0) (i_immediate 0) (j_immediate 0) (s_immediate 0)
         (u_immediate 0) (b_immediate 0) (load_address 0) (store_address 0)
         (is_ecall 0) (is_csr 0)
         (alu_specifics ((subtract_instead_of_add 0) (arithmetic_shift 0)))
         (error 0)))
       (transaction ((set_rd 1) (new_rd 4) (new_pc 4) (error 0))) (error 0)
       (is_ecall 0) (write_bus ((valid 0) (data ((address 0) (write_data 0)))))
       (read_bus ((valid 0) (data ((address 0))))))
      |}])
;;
