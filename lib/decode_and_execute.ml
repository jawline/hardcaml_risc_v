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
  module Transaction = Transaction.Make (Hart_config)
  module Op = Op.Make (Hart_config) (Memory) (Decoded_instruction)

  let register_width = Register_width.bits Hart_config.register_width

  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; memory_controller_to_hart : 'a Memory.Rx_bus.Tx.t
           [@rtlprefix "memory_controller_to_hart"]
      ; hart_to_memory_controller : 'a Memory.Tx_bus.Rx.t
           [@rtlprefix "hart_to_memory_controller"]
      ; enable : 'a
      ; instruction : 'a [@bits register_width]
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

  let op_imm_instructions
    ~(registers : _ Registers.t)
    scope
    ({ funct3; funct7; rs1; i_immediate; _ } : _ Decoded_instruction.t)
    =
    let { Op.O.rd = new_rd; error } =
      (* TODO: A mux could allow us to use the same mux for both *)
      (* TODO: The mux could be on the decode cycle into decoded
         instruction and used by both. *)
      Op.hierarchical
        ~instance:"op_imm"
        scope
        { Op.I.funct3; funct7; lhs = rs1; rhs = i_immediate }
    in
    { Transaction.set_rd = one 1; new_rd; error; new_pc = registers.pc +:. 4 }
  ;;

  let op_instructions
    ~(registers : _ Registers.t)
    scope
    ({ funct3; funct7; rs1; rs2; _ } : _ Decoded_instruction.t)
    =
    let { Op.O.rd = new_rd; error } =
      Op.hierarchical ~instance:"op" scope { Op.I.funct3; funct7; lhs = rs1; rhs = rs2 }
    in
    { Transaction.set_rd = one 1; new_rd; error; new_pc = registers.pc +:. 4 }
  ;;

  (** JAL (jump and link) adds the signed J-immediate value to the current PC
      after storing the current PC + 4 in the destination register. *)
  let jal_instruction
    ~(registers : _ Registers.t)
    (decoded_instruction : _ Decoded_instruction.t)
    =
    let new_pc = registers.pc +: decoded_instruction.j_immediate in
    let error = new_pc &:. 0b11 <>:. 0 in
    { Transaction.set_rd = one 1; new_rd = registers.pc +:. 4; new_pc; error }
  ;;

  (** JALR (Indirect jump) adds a 12-bit signed immediate to whatever is at rs1,
      sets the LSB of that result to zero (e.g, result = result & (!1)), and
      finally sets the PC to this new result.  rd is set to the original PC + 4
      (the start of the next instruction).  Regiser 0 can be used to discard the
      result. *)
  let jalr_instruction
    ~(registers : _ Registers.t)
    (decoded_instruction : _ Decoded_instruction.t)
    =
    let new_pc =
      decoded_instruction.rs1 +: decoded_instruction.j_immediate
      &: ~:(of_int ~width:register_width 1)
    in
    let error = new_pc &:. 0b11 <>:. 0 in
    { Transaction.set_rd = one 1; new_pc; error; new_rd = registers.pc +:. 4 }
  ;;

  (** LUI (load upper immediate) sets rd to the decoded U immediate (20 bit
      value from the msb with zeros for the lower 12. *)
  let lui_instruction
    ~(registers : _ Registers.t)
    (decoded_instruction : _ Decoded_instruction.t)
    =
    { Transaction.set_rd = one 1
    ; new_rd = registers.pc +:. 4
    ; error = zero 1
    ; new_pc = decoded_instruction.u_immediate
    }
  ;;

  (** Add upper immediate to PC. Similar to LUI but adds the loaded immediate to
      current the program counter and places it in RD. This can be used to compute
      addresses for JALR instructions. *)
  let auipc_instruction
    ~(registers : _ Registers.t)
    (decoded_instruction : _ Decoded_instruction.t)
    =
    { Transaction.set_rd = one 1
    ; new_rd = registers.pc +:. 4
    ; error = zero 1
    ; new_pc = registers.pc +: decoded_instruction.u_immediate
    }
  ;;

  let fence ~(registers : _ Registers.t) (_decoded_instruction : _ Decoded_instruction.t) =
    (* TODO: Currently all memory transactions are atomic so I'm not sure if I
     * need to implement this. Figure it out. *)
    { Transaction.set_rd = one 1
    ; new_rd = zero register_width
    ; error = zero 1
    ; new_pc = registers.pc +:. 4
    }
  ;;

  module Table_entry = struct
    type 'a t =
      { opcode : int
      ; new_pc : 'a
      ; set_rd : 'a
      ; new_rd : 'a
      ; error : 'a
      }

    let create ~opcode { Transaction.new_pc; set_rd; new_rd; error } =
      { opcode; new_pc; set_rd; new_rd; error }
    ;;
  end

  let instruction_table ~registers ~decoded_instruction scope =
    [ Table_entry.create
        ~opcode:Opcodes.op
        (op_instructions ~registers scope decoded_instruction)
    ; Table_entry.create
        ~opcode:Opcodes.op_imm
        (op_imm_instructions ~registers scope decoded_instruction)
    ; Table_entry.create
        ~opcode:Opcodes.jal
        (jal_instruction ~registers decoded_instruction)
    ; Table_entry.create
        ~opcode:Opcodes.jalr
        (jalr_instruction ~registers decoded_instruction)
    ; Table_entry.create
        ~opcode:Opcodes.lui
        (lui_instruction ~registers decoded_instruction)
    ; Table_entry.create
        ~opcode:Opcodes.auipc
        (auipc_instruction ~registers decoded_instruction)
    ; Table_entry.create ~opcode:Opcodes.fence
        (fence ~registers decoded_instruction)
    ]
  ;;

  module State = struct
    type t =
      | Decoding
      | Executing
      | Committing
    [@@deriving sexp_of, compare, enumerate]
  end

  let create scope (i : _ I.t) =
    let reg_spec = Reg_spec.create ~clear:i.clear ~clock:i.clock () in
    let decoded_instruction = Decoded_instruction.Of_always.reg reg_spec in
    let instruction_table =
      instruction_table
        ~decoded_instruction:(Decoded_instruction.Of_always.value decoded_instruction)
        ~registers:i.registers
        scope
    in
    let transaction = Transaction.Of_always.reg reg_spec in
    (* TODO: Staging the muxes into and out of registers might make this slightly cheaper *)
    let current_state = State_machine.create (module State) reg_spec in
    (* TODO: Register 0 is always zero, enforce that here. *)
    let commit_transaction ~new_pc ~set_rd ~new_rd =
      { Registers.pc = new_pc
      ; general =
          List.mapi
            ~f:(fun index current_value ->
              mux2
                (set_rd &: (decoded_instruction.rd.value ==:. index))
                new_rd
                current_value)
            i.registers.general
      }
    in
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
                , List.map
                    ~f:(fun { Table_entry.opcode; new_pc; set_rd; new_rd; error } ->
                      when_
                        (decoded_instruction.opcode.value ==:. opcode)
                        [ Transaction.Of_always.assign
                            transaction
                            { new_pc; set_rd; new_rd; error }
                        ; current_state.set_next Committing
                        ])
                    instruction_table )
                (* TODO: Trap when no opcode matches *)
              ; Committing, [ current_state.set_next Decoding ]
              ]
          ]
      ];
    { O.memory_controller_to_hart = Memory.Rx_bus.Rx.Of_signal.of_int 0
    ; hart_to_memory_controller = Memory.Tx_bus.Tx.Of_signal.of_int 0
    ; finished = current_state.is Committing
    ; new_registers =
        commit_transaction
          ~new_pc:transaction.new_pc.value
          ~set_rd:transaction.set_rd.value
          ~new_rd:transaction.new_rd.value
    ; error = transaction.error.value
    }
  ;;

  let hierarchical ~instance (scope : Scope.t) (input : Signal.t I.t) =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ~scope ~name:"Decode_and_execute" ~instance create input
  ;;
end
