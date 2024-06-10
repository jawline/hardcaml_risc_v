open! Core
open Hardcaml
open Hardcaml_risc_v
open Hardcaml_risc_v_hart
open Hardcaml_uart_controller
open Hardcaml_waveterm
open Opcode_helper
open! Bits

let debug = false

module Make (M : sig
    type sim

    val create_sim : string -> sim
    val finalize_sim : sim -> unit
    val test : instructions:Bits.t list -> sim -> unit
  end) =
struct
  let create_sim = M.create_sim
  let finalize_sim = M.finalize_sim
  let test = M.test

  let%expect_test "op_imm" =
    let sim = create_sim "test_op_imm" in
    test
      ~instructions:[ op_imm ~funct3:Funct3.Op.Add_or_sub ~rs1:0 ~rd:1 ~immediate:550 ]
      sim;
    [%expect
      {|
    ((pc 4)
     (general
      (0 550 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))
    22600093 00 00 00 00 00 00 00 |}];
    test
      ~instructions:
        [ op_imm ~funct3:Funct3.Op.Xor ~rs1:0 ~rd:1 ~immediate:0b0101
        ; op_imm ~funct3:Funct3.Op.Xor ~rs1:1 ~rd:1 ~immediate:0b1010
        ]
      sim;
    [%expect
      {|
     ((pc 8)
      (general (0 15 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))
     504093 a0c093 00 00 00 00 00 00 |}];
    test
      ~instructions:
        [ op_imm ~funct3:Funct3.Op.Add_or_sub ~rs1:0 ~rd:1 ~immediate:0b1111
        ; op_imm ~funct3:Funct3.Op.And ~rs1:1 ~rd:1 ~immediate:0b11
        ]
      sim;
    [%expect
      {|
     ((pc 8)
      (general (0 3 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))
     f00093 30f093 00 00 00 00 00 00 |}];
    test
      ~instructions:
        [ op_imm ~funct3:Funct3.Op.Add_or_sub ~rs1:0 ~rd:1 ~immediate:0b101010
        ; op_imm ~funct3:Funct3.Op.Or ~rs1:1 ~rd:1 ~immediate:0b010101
        ]
      sim;
    [%expect
      {|
     ((pc 8)
      (general (0 63 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))
     2a00093 150e093 00 00 00 00 00 00 |}];
    test
      ~instructions:
        [ op_imm ~funct3:Funct3.Op.Add_or_sub ~rs1:0 ~rd:1 ~immediate:0b1
        ; op_imm ~funct3:Funct3.Op.Sll ~rs1:1 ~rd:1 ~immediate:4
        ]
      sim;
    [%expect
      {|
     ((pc 8)
      (general (0 16 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))
     100093 409093 00 00 00 00 00 00 |}];
    (* TODO: Test the negative cases *)
    test
      ~instructions:
        [ op_imm ~funct3:Funct3.Op.Add_or_sub ~rs1:0 ~rd:1 ~immediate:10
        ; op_imm ~funct3:Funct3.Op.Slt ~rs1:1 ~rd:2 ~immediate:5
        ; op_imm ~funct3:Funct3.Op.Slt ~rs1:1 ~rd:3 ~immediate:15
        ]
      sim;
    [%expect
      {|
     ((pc 12)
      (general (0 10 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))
     a00093 50a113 f0a193 00 00 00 00 00 |}];
    test
      ~instructions:
        [ op_imm ~funct3:Funct3.Op.Add_or_sub ~rs1:0 ~rd:1 ~immediate:10
        ; op_imm ~funct3:Funct3.Op.Sltu ~rs1:1 ~rd:2 ~immediate:5
        ; op_imm ~funct3:Funct3.Op.Sltu ~rs1:1 ~rd:3 ~immediate:15
        ]
      sim;
    [%expect
      {|
     ((pc 12)
      (general (0 10 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))
     a00093 50b113 f0b193 00 00 00 00 00 |}];
    test
      ~instructions:
        [ op_imm ~funct3:Funct3.Op.Add_or_sub ~rs1:0 ~rd:1 ~immediate:16
        ; op_imm ~funct3:Funct3.Op.Srl_or_sra ~rs1:1 ~rd:2 ~immediate:4
        ; op_imm ~funct3:Funct3.Op.Srl_or_sra ~rs1:1 ~rd:3 ~immediate:3
        ]
      sim;
    [%expect
      {|
     ((pc 12)
      (general (0 16 1 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))
     1000093 40d113 30d193 00 00 00 00 00 |}];
    (* Test x0 is hardwired to zero. *)
    test
      ~instructions:[ op_imm ~funct3:Funct3.Op.Add_or_sub ~rd:0 ~rs1:0 ~immediate:0x500 ]
      sim;
    [%expect
      {|
     ((pc 4)
      (general (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))
     50000013 00 00 00 00 00 00 00 |}];
    M.finalize_sim sim;
    [%expect {| |}]
  ;;

  let%expect_test "op_imm" =
    let sim = create_sim "op" in
    test
      ~instructions:
        [ op_imm ~funct3:Funct3.Op.Add_or_sub ~rs1:0 ~rd:1 ~immediate:500
        ; op_imm ~funct3:Funct3.Op.Add_or_sub ~rs1:0 ~rd:2 ~immediate:300
        ; op ~funct3:Funct3.Op.Add_or_sub ~rs1:1 ~rs2:2 ~rd:3 (* WRONG *) ~funct7:0
        ]
      sim;
    [%expect
      {|
    ((pc 12)
     (general
      (0 500 300 200 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))
    1f400093 12c00113 2081b3 00 00 00 00 00 |}];
    M.finalize_sim sim;
    [%expect {| |}]
  ;;

  let%expect_test "branch" =
    let sim = create_sim "branch" in
    test
      ~instructions:
        [ branch ~funct3:Funct3.Branch.Bne ~rs1:0 ~rs2:0 ~offset:200
        ; branch ~funct3:Funct3.Branch.Beq ~rs1:0 ~rs2:0 ~offset:500
        ]
      sim;
    [%expect
      {|
    ((pc 504)
     (general (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))
    c001463 1e000a63 00 00 00 00 00 00 |}];
    test
      ~instructions:
        [ op_imm ~funct3:Funct3.Op.Add_or_sub ~rs1:0 ~rd:1 ~immediate:550
        ; branch ~funct3:Funct3.Branch.Beq ~rs1:0 ~rs2:1 ~offset:500
        ]
      sim;
    [%expect
      {|
      ((pc 8)
       (general
        (0 550 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))
      22600093 1e100a63 00 00 00 00 00 00 |}];
    test
      ~instructions:
        [ op_imm ~funct3:Funct3.Op.Add_or_sub ~rs1:0 ~rd:1 ~immediate:550
        ; branch ~funct3:Funct3.Branch.Bne ~rs1:0 ~rs2:1 ~offset:500
        ]
      sim;
    [%expect
      {|
      ((pc 504)
       (general
        (0 550 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))
      22600093 1e101a63 00 00 00 00 00 00 |}];
    (* Expect the second branch to be taken *)
    test
      ~instructions:
        [ branch ~funct3:Funct3.Branch.Blt ~rs1:0 ~rs2:1 ~offset:50
        ; op_imm ~funct3:Funct3.Op.Add_or_sub ~rs1:0 ~rd:1 ~immediate:100
        ; branch ~funct3:Funct3.Branch.Blt ~rs1:0 ~rs2:1 ~offset:250
        ]
      sim;
    [%expect
      {|
      ((pc 258)
       (general
        (0 100 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))
      2104963 6400093 e104d63 00 00 00 00 00 |}];
    (* Expect the second branch to be taken *)
    test
      ~instructions:
        [ op_imm ~funct3:Funct3.Op.Add_or_sub ~rs1:0 ~rd:1 ~immediate:100
        ; branch ~funct3:Funct3.Branch.Bge ~rs1:0 ~rs2:1 ~offset:50
        ; branch ~funct3:Funct3.Branch.Bge ~rs1:1 ~rs2:0 ~offset:250
        ]
      sim;
    [%expect
      {|
      ((pc 258)
       (general
        (0 100 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))
      6400093 2105963 e00dd63 00 00 00 00 00 |}];
    (* TODO: Test unsigned variants and negative numbers. *)
    M.finalize_sim sim;
    [%expect {| |}]
  ;;
end

module Cpu_with_no_io_controller =
  Cpu.Make
    (struct
      let register_width = Register_width.B32
      let num_registers = 32
    end)
    (struct
      let num_bytes = 32
    end)
    (struct
      let num_harts = 1
      let include_io_controller = Io_controller_config.No_io_controller
    end)

module With_manually_programmed_ram = Make (struct
    type sim =
      ( Bits.t ref Cpu_with_no_io_controller.I.t
        , Bits.t ref Cpu_with_no_io_controller.O.t )
        Cyclesim.t
      * Waveform.t
      * string

    let create_sim name : sim =
      let module Sim =
        Cyclesim.With_interface
          (Cpu_with_no_io_controller.I)
          (Cpu_with_no_io_controller.O)
      in
      let sim =
        Sim.create
          ~config:Cyclesim.Config.trace_all
          (Cpu_with_no_io_controller.create
             (Scope.create ~auto_label_hierarchical_ports:true ~flatten_design:true ()))
      in
      let waveform, sim = Waveform.create sim in
      sim, waveform, name
    ;;

    let finalize_sim sim =
      if debug
      then (
        let _, waveform, name = sim in
        Waveform.Serialize.marshall waveform ("/tmp/" ^ name))
      else ()
    ;;

    let print_ram sim =
      let ram = Cyclesim.lookup_mem sim "main_memory_bram" |> Option.value_exn in
      Array.map ~f:(fun mut -> Bits.Mutable.to_bits mut |> Bits.to_int) ram
      |> Array.iter ~f:(fun v -> printf "%02x " v);
      printf "\n"
    ;;

    let clear_registers ~(inputs : Bits.t ref Cpu_with_no_io_controller.I.t) sim =
      inputs.clear := Bits.one 1;
      Cyclesim.cycle sim;
      inputs.clear := Bits.zero 1
    ;;

    let test ~instructions sim =
      let sim, _, _ = sim in
      (* Initialize the main memory to some known values for testing. *)
      let initial_ram = Cyclesim.lookup_mem sim "main_memory_bram" |> Option.value_exn in
      Array.iter
        ~f:(fun tgt -> Bits.Mutable.copy_bits ~src:(Bits.of_int ~width:8 0) ~dst:tgt)
        initial_ram;
      Array.iteri
        ~f:(fun index tgt ->
          match List.nth instructions index with
          | Some instruction -> Bits.Mutable.copy_bits ~src:instruction ~dst:tgt
          | None -> ())
        initial_ram;
      let inputs : _ Cpu_with_no_io_controller.I.t = Cyclesim.inputs sim in
      let _outputs_before : _ Cpu_with_no_io_controller.O.t =
        Cyclesim.outputs ~clock_edge:Side.Before sim
      in
      let outputs : _ Cpu_with_no_io_controller.O.t = Cyclesim.outputs sim in
      clear_registers ~inputs sim;
      let rec loop_for cycles =
        if cycles = 0
        then ()
        else (
          Cyclesim.cycle sim;
          loop_for (cycles - 1))
      in
      loop_for 50;
      match outputs.registers with
      | [ outputs ] ->
        let outputs =
          Cpu_with_no_io_controller.Registers.map ~f:(fun t -> Bits.to_int !t) outputs
        in
        print_s [%message "" ~_:(outputs : int Cpu_with_no_io_controller.Registers.t)];
        print_ram sim
      | _ -> raise_s [%message "BUG: Unexpected number of harts"]
    ;;
  end)

let uart_config =
  { Hardcaml_uart_controller.Config.clock_frequency = 200
  ; baud_rate = 200
  ; include_parity_bit = true
  ; stop_bits = 1
  }
;;

module Uart_tx = Uart_tx.Make (struct
    let config = uart_config
  end)

module Cpu_with_dma_memory =
  Cpu.Make
    (struct
      let register_width = Register_width.B32
      let num_registers = 32
    end)
    (struct
      let num_bytes = 32
    end)
    (struct
      let num_harts = 1
      let include_io_controller = Io_controller_config.Uart_controller uart_config
    end)

module With_transmitter = struct
  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; data_in_valid : 'a
      ; data_in : 'a [@bits 8]
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t = { registers : 'a Cpu_with_dma_memory.Registers.t list [@length 1] }
    [@@deriving sexp_of, hardcaml]
  end

  let create scope { I.clock; clear; data_in_valid; data_in } =
    let { Uart_tx.O.uart_tx; _ } =
      Uart_tx.hierarchical
        ~instance:"tx"
        scope
        { Uart_tx.I.clock; clear; data_in_valid; data_in }
    in
    let { Cpu_with_dma_memory.O.registers; _ } =
      Cpu_with_dma_memory.hierarchical
        ~instance:"cpu"
        scope
        { clock; clear; uart_rx = uart_tx }
    in
    { O.registers }
  ;;
end

module With_dma_ram = Make (struct
    type sim =
      (Bits.t ref With_transmitter.I.t, Bits.t ref With_transmitter.O.t) Cyclesim.t
      * Waveform.t
      * string

    let create_sim name : sim =
      let module Sim = Cyclesim.With_interface (With_transmitter.I) (With_transmitter.O)
      in
      let sim =
        Sim.create
          ~config:Cyclesim.Config.trace_all
          (With_transmitter.create
             (Scope.create ~auto_label_hierarchical_ports:true ~flatten_design:true ()))
      in
      let waveform, sim = Waveform.create sim in
      sim, waveform, name
    ;;

    let finalize_sim sim =
      if debug
      then (
        let _, waveform, name = sim in
        Waveform.Serialize.marshall waveform ("/tmp/dma_" ^ name))
      else ()
    ;;

    let print_ram sim =
      let ram = Cyclesim.lookup_mem sim "main_memory_bram" |> Option.value_exn in
      Array.map ~f:(fun mut -> Bits.Mutable.to_bits mut |> Bits.to_int) ram
      |> Array.iter ~f:(fun v -> printf "%02x " v);
      printf "\n"
    ;;

    let clear_registers ~(inputs : Bits.t ref With_transmitter.I.t) sim =
      inputs.clear := Bits.one 1;
      Cyclesim.cycle sim;
      inputs.clear := Bits.zero 1
    ;;

    let send_bits sim whole_packet =
      (* Send the message through byte by byte. Uart_tx will transmit a
         * byte once every ~10 cycles (this is dependent on the number of stop
         * bits and the parity bit. *)
      let inputs : _ With_transmitter.I.t = Cyclesim.inputs sim in
      let rec loop_for n =
        if n = 0
        then ()
        else (
          Cyclesim.cycle sim;
          loop_for (n - 1))
      in
      List.iter
        ~f:(fun input ->
          inputs.data_in_valid := vdd;
          inputs.data_in := of_int ~width:8 input;
          Cyclesim.cycle sim;
          inputs.data_in_valid := of_int ~width:1 0;
          (* TODO: Tighter loop *)
          loop_for 20)
        whole_packet
    ;;

    let send_dma_message ~address ~packet sim =
      (* TODO: Move this to a util section *)
      let whole_packet = dma_packet ~address packet in
      send_bits sim whole_packet
    ;;

    let test ~instructions sim =
      let sim, _, _ = sim in
      let inputs : _ With_transmitter.I.t = Cyclesim.inputs sim in
      (* Initialize the main memory to some known values for testing. *)
      let initial_ram = Cyclesim.lookup_mem sim "main_memory_bram" |> Option.value_exn in
      Array.iter
        ~f:(fun tgt -> Bits.Mutable.copy_bits ~src:(Bits.of_int ~width:8 0) ~dst:tgt)
        initial_ram;
      (* Send a clear signal to initialize any CPU IO controller state back to
       * default so we're ready to receive. *)
      clear_registers ~inputs sim;
      send_dma_message
        ~address:0
        ~packet:
          (Bits.concat_lsb instructions
           |> Bits.split_lsb ~part_width:8
           |> List.map ~f:Bits.to_char
           |> String.of_char_list)
        sim;
      send_bits sim clear_packet;
      let _outputs_before : _ With_transmitter.O.t =
        Cyclesim.outputs ~clock_edge:Side.Before sim
      in
      let outputs : _ With_transmitter.O.t = Cyclesim.outputs sim in
      let rec loop_for cycles =
        if cycles = 0
        then ()
        else (
          Cyclesim.cycle sim;
          loop_for (cycles - 1))
      in
      loop_for 50;
      match outputs.registers with
      | [ outputs ] ->
        let outputs =
          Cpu_with_dma_memory.Registers.map ~f:(fun t -> Bits.to_int !t) outputs
        in
        print_s [%message "" ~_:(outputs : int Cpu_with_dma_memory.Registers.t)];
        print_ram sim
      | _ -> raise_s [%message "BUG: Unexpected number of harts"]
    ;;
  end)
