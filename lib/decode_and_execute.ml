open! Core
open Hardcaml
open Signal
open Always

module Make
    (Hart_config : Hart_config_intf.S)
    (Memory : Memory_bus_intf.S)
    (Registers : Registers_intf.S) =
struct
  module Op = Op.Make (Hart_config) (Memory) (Registers)

  module I = struct
    type 'a t =
      { memory_controller_to_hart : 'a Memory.Rx_bus.Tx.t
           [@rtlprefix "memory_controller_to_hart"]
      ; hart_to_memory_controller : 'a Memory.Tx_bus.Rx.t
           [@rtlprefix "hart_to_memory_controller"]
      ; enable : 'a
      ; instruction : 'a
           (* TODO: This is assuming Rv32i, I guess in practice this should be the length of the longest instruction we support? *)
           [@bits 32]
      ; registers : 'a Registers.t [@rtlprefix "input_registers"]
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t =
      { memory_controller_to_hart : 'a Memory.Rx_bus.Rx.t
           [@rtlprefix "memory_controller_to_hart"]
      ; hart_to_memory_controller : 'a Memory.Tx_bus.Tx.t
           [@rtlprefix "hart_to_memory_controller"]
      ; finished : 'a
      ; new_registers : 'a Registers.t [@rtlprefix "output_registers"]
      ; error : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  (** To make sure we actually select the register rather than make a mistake and
      use the signal in the instruction mint a new type to carry the signal *)
  module Selected_register = struct
    type 'a t = { value : 'a }
  end

  let select_register (registers : _ Registers.t) slot =
    { Selected_register.value = mux slot registers.general }
  ;;

  (* TODO: Register 0 is always zero, enforce that here. *)
  let assign_register (registers : _ Registers.t) slot new_value =
    { Registers.pc = registers.pc
    ; general =
        List.mapi
          ~f:(fun index current_value -> mux2 (slot ==:. index) new_value current_value)
          registers.general
    }
  ;;

  let increment_pc (registers : _ Registers.t) =
    { registers with pc = registers.pc +:. 4 }
  ;;

  let op_instructions scope (registers : _ Registers.t) (instruction : Signal.t) =
    (* TODO: Staging the muxes nto and out of registers might make this slightly cheaper *)
    let rs1 : _ Selected_register.t =
      select_register registers (Decoder.rs1 instruction)
    in
    let rs2 : _ Selected_register.t =
      select_register registers (Decoder.rs2 instruction)
    in
    let funct3 = Decoder.funct3 instruction in
    let funct7 = Decoder.funct7 instruction in
    let { Op.O.rd = new_rd; error } =
      Op.hierarchical
        ~instance:"op"
        scope
        { Op.I.rs1 = rs1.value; rs2 = rs2.value; funct3; funct7 }
    in
    let new_registers =
      assign_register registers (Decoder.rd instruction) new_rd |> increment_pc
    in
    new_registers, error
  ;;

  let create scope (i : _ I.t) =
    let finished = Variable.wire ~default:(zero 1) in
    let new_registers = Registers.Of_always.wire zero in
    let is_error = Variable.wire ~default:(zero 1) in
    let op_instruction, op_error = op_instructions scope i.registers i.instruction in
    compile
      [ when_
          i.enable
          [ when_
              (Decoder.opcode i.instruction ==:. Opcodes.op)
              [ Registers.Of_always.assign new_registers op_instruction
              ; is_error <-- op_error
              ; finished <--. 1
              ]
          ]
      ];
    { O.memory_controller_to_hart = Memory.Rx_bus.Rx.Of_signal.of_int 0
    ; hart_to_memory_controller = Memory.Tx_bus.Tx.Of_signal.of_int 0
    ; finished = finished.value
    ; new_registers = Registers.Of_always.value new_registers
    ; error = is_error.value
    }
  ;;

  let hierarchical ~instance (scope : Scope.t) (input : Signal.t I.t) =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ~scope ~name:"Decode_and_execute" ~instance create input
  ;;
end
