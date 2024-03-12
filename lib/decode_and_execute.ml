open! Core
open Hardcaml
open Signal
open Always

module Make
    (Hart_config : Hart_config_intf.S)
    (Memory : Memory_bus_intf.S)
    (Registers : Registers_intf.S) =
struct
  module Decoded_instruction = Decoded_instruction.Make (Hart_config) (Registers)
  module Op = Op.Make (Hart_config) (Memory) (Decoded_instruction)

  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; memory_controller_to_hart : 'a Memory.Rx_bus.Tx.t
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

  let op_instructions ~registers scope (decoded_instruction : _ Decoded_instruction.t) =
    (* TODO: Staging the muxes nto and out of registers might make this slightly cheaper *)
    let { Op.O.rd = new_rd; error } =
      Op.hierarchical ~instance:"op" scope decoded_instruction
    in
    let new_registers =
      assign_register registers decoded_instruction.rd new_rd |> increment_pc
    in
    new_registers, error
  ;;

  module State = struct
    type t =
      | Decoding
      | Executing
    [@@deriving sexp_of, compare, enumerate]
  end

  let create scope (i : _ I.t) =
    let reg_spec = Reg_spec.create ~clear:i.clear ~clock:i.clock () in
    let finished = Variable.wire ~default:(zero 1) in
    let new_registers = Registers.Of_always.wire zero in
    let is_error = Variable.wire ~default:(zero 1) in
    let decoded_instruction = Decoded_instruction.Of_always.reg reg_spec in
    let op_instruction, op_error =
      op_instructions
        ~registers:i.registers
        scope
        (Decoded_instruction.Of_always.value decoded_instruction)
    in
    let current_state = State_machine.create (module State) reg_spec in
    compile
      [ when_
          i.enable
          [ current_state.switch
              [ ( State.Decoding
                , [ Decoded_instruction.Of_always.assign
                      decoded_instruction
                      (Decoded_instruction.of_instruction i.instruction i.registers)
                  ; current_state.set_next Executing
                  ] )
              ; ( State.Executing
                , [ when_
                      (Decoder.opcode i.instruction ==:. Opcodes.op)
                      [ Registers.Of_always.assign new_registers op_instruction
                      ; is_error <-- op_error
                      ; finished <--. 1
                      ; current_state.set_next Decoding
                      ]
                  ] )
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
