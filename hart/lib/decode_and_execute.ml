open! Core
open Hardcaml
open Hardcaml_memory_controller
open Signal
open Always

module Make
    (Hart_config : Hart_config_intf.S)
    (Memory : Memory_bus_intf.S)
    (Registers : Registers_intf.S)
    (Decoded_instruction : Decoded_instruction_intf.M(Registers).S)
    (Transaction : Transaction_intf.S) =
struct
  module Opcode_output = Opcode_output.Make (Hart_config) (Memory) (Transaction)
  module Op = Op.Make (Hart_config)
  module Branch = Branch.Make (Hart_config)
  module Load = Load.Make (Hart_config) (Memory)
  module Store = Store.Make (Hart_config) (Memory)

  let register_width = Register_width.bits Hart_config.register_width

  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; memory_controller_to_hart : 'a Memory.Rx_bus.Tx.t
      ; hart_to_memory_controller : 'a Memory.Tx_bus.Rx.t
      ; instruction : 'a [@bits register_width]
      ; registers : 'a Registers.t
      ; ecall_transaction : 'a Transaction.t
      ; process_instruction : 'a
      (** Set high when decode and execute should process an instruction. *)
      }
    [@@deriving sexp_of, hardcaml ~rtlmangle:"$"]
  end

  module O = struct
    type 'a t =
      { memory_controller_to_hart : 'a Memory.Rx_bus.Rx.t
      ; hart_to_memory_controller : 'a Memory.Tx_bus.Tx.t
      ; finished : 'a
      ; new_registers : 'a Registers.For_writeback.t [@rtlprefix "new_registers$"]
      ; error : 'a
      ; is_ecall : 'a
      }
    [@@deriving sexp_of, hardcaml ~rtlmangle:"$"]
  end

  let op_imm_instructions
    ~(registers : _ Registers.t)
    scope
    ({ funct3; funct7; rs1; i_immediate; _ } : _ Decoded_instruction.t)
    =
    let { Op.O.rd = new_rd; error } =
      Op.hierarchical (* There is no SUBI since a signed addi is sufficient. *)
        ~enable_subtract:false
        ~instance:"op_imm"
        scope
        { Op.I.funct3; funct7; lhs = rs1; rhs = i_immediate }
    in
    { Opcode_output.transaction =
        { Transaction.finished = vdd
        ; set_rd = vdd
        ; new_rd
        ; error
        ; new_pc = registers.pc +:. 4
        }
    ; memory_controller_to_hart = Memory.Rx_bus.Rx.Of_signal.of_int 0
    ; hart_to_memory_controller = Memory.Tx_bus.Tx.Of_signal.of_int 0
    }
  ;;

  let op_instructions
    ~(registers : _ Registers.t)
    scope
    ({ funct3; funct7; rs1; rs2; _ } : _ Decoded_instruction.t)
    =
    let { Op.O.rd = new_rd; error } =
      Op.hierarchical
        ~instance:"op"
        ~enable_subtract:true
        scope
        { Op.I.funct3; funct7; lhs = rs1; rhs = rs2 }
    in
    { Opcode_output.transaction =
        { Transaction.finished = vdd
        ; set_rd = vdd
        ; new_rd
        ; error
        ; new_pc = registers.pc +:. 4
        }
    ; memory_controller_to_hart = Memory.Rx_bus.Rx.Of_signal.of_int 0
    ; hart_to_memory_controller = Memory.Tx_bus.Tx.Of_signal.of_int 0
    }
  ;;

  (** JAL (jump and link) adds the signed J-immediate value to the current PC
      after storing the current PC + 4 in the destination register. *)
  let jal_instruction
    ~(registers : _ Registers.t)
    (decoded_instruction : _ Decoded_instruction.t)
    =
    let new_pc = registers.pc +: decoded_instruction.j_immediate in
    let error = new_pc &:. 0b11 <>:. 0 in
    { Opcode_output.transaction =
        { Transaction.finished = vdd
        ; set_rd = vdd
        ; new_rd = registers.pc +:. 4
        ; new_pc
        ; error
        }
    ; memory_controller_to_hart = Memory.Rx_bus.Rx.Of_signal.of_int 0
    ; hart_to_memory_controller = Memory.Tx_bus.Tx.Of_signal.of_int 0
    }
  ;;

  (** JALR (Indirect jump) adds a 12-bit signed immediate to whatever is at rs1,
      sets the LSB of that result to zero (e.g, result = result & (!1)), and
      finally sets the PC to this new result.  rd is set to the original PC + 4
      (the start of the next instruction).  Regiser 0 can be used to discard the
      result. *)
  let jalr_instruction
    ~(registers : _ Registers.t)
    (decoded_instruction : _ Decoded_instruction.t)
    scope
    =
    let ( -- ) = Scope.naming scope in
    let new_pc =
      decoded_instruction.rs1
      -- "jalr_rs1"
      +: (decoded_instruction.i_immediate -- "jalr_i_immediate")
      -- "jalr_new_pc"
      &: ~:(of_int ~width:register_width 1)
    in
    let error = new_pc &:. 0b11 <>:. 0 in
    { Opcode_output.transaction =
        { Transaction.finished = vdd
        ; set_rd = vdd
        ; new_pc
        ; error
        ; new_rd = registers.pc +:. 4
        }
    ; memory_controller_to_hart = Memory.Rx_bus.Rx.Of_signal.of_int 0
    ; hart_to_memory_controller = Memory.Tx_bus.Tx.Of_signal.of_int 0
    }
  ;;

  (** LUI (load upper immediate) sets rd to the decoded U immediate (20 bit
      value from the msb with zeros for the lower 12. *)
  let lui_instruction
    ~(registers : _ Registers.t)
    (decoded_instruction : _ Decoded_instruction.t)
    =
    { Opcode_output.transaction =
        { Transaction.finished = vdd
        ; set_rd = vdd
        ; new_rd = decoded_instruction.u_immediate
        ; error = gnd
        ; new_pc = registers.pc +:. 4
        }
    ; memory_controller_to_hart = Memory.Rx_bus.Rx.Of_signal.of_int 0
    ; hart_to_memory_controller = Memory.Tx_bus.Tx.Of_signal.of_int 0
    }
  ;;

  (** Add upper immediate to PC. Similar to LUI but adds the loaded immediate to
      current the program counter and places it in RD. This can be used to compute
      addresses for JALR instructions. *)
  let auipc_instruction
    ~(registers : _ Registers.t)
    (decoded_instruction : _ Decoded_instruction.t)
    =
    { Opcode_output.transaction =
        { Transaction.finished = vdd
        ; set_rd = vdd
        ; new_rd = registers.pc +:. 4
        ; error = gnd
        ; new_pc = registers.pc +: decoded_instruction.u_immediate
        }
    ; memory_controller_to_hart = Memory.Rx_bus.Rx.Of_signal.of_int 0
    ; hart_to_memory_controller = Memory.Tx_bus.Tx.Of_signal.of_int 0
    }
  ;;

  (** The branch table either compares rs1 and rs2 based on funct3 and then
      either adds a b immediate to the PC or skips to the next instruction
      based on the result. *)
  let branch_instruction
    ~(registers : _ Registers.t)
    (decoded_instruction : _ Decoded_instruction.t)
    scope
    =
    let { Branch.O.new_pc; error } =
      Branch.hierarchical
        ~instance:"branch"
        scope
        { Branch.I.funct3 = decoded_instruction.funct3
        ; lhs = decoded_instruction.rs1
        ; rhs = decoded_instruction.rs2
        ; b_immediate = decoded_instruction.b_immediate
        ; pc = registers.pc
        }
    in
    { Opcode_output.transaction =
        { Transaction.finished = vdd
        ; set_rd = gnd
        ; new_rd = zero register_width
        ; error
        ; new_pc
        }
    ; memory_controller_to_hart = Memory.Rx_bus.Rx.Of_signal.of_int 0
    ; hart_to_memory_controller = Memory.Tx_bus.Tx.Of_signal.of_int 0
    }
  ;;

  let fence ~(registers : _ Registers.t) (_decoded_instruction : _ Decoded_instruction.t) =
    (* TODO: Currently all memory transactions are atomic so I'm not sure if I
     * need to implement this. Figure it out. *)
    { Opcode_output.transaction =
        { Transaction.finished = vdd
        ; set_rd = vdd
        ; new_rd = zero register_width
        ; error = gnd
        ; new_pc = registers.pc +:. 4
        }
    ; memory_controller_to_hart = Memory.Rx_bus.Rx.Of_signal.of_int 0
    ; hart_to_memory_controller = Memory.Tx_bus.Tx.Of_signal.of_int 0
    }
  ;;

  (** The load table loads a value from [rs1] and places it in rd *)
  let load_instruction
    ~clock
    ~clear
    ~memory_controller_to_hart
    ~hart_to_memory_controller
    ~(registers : _ Registers.t)
    ~(enables : _ Enables.t)
    (decoded_instruction : _ Decoded_instruction.t)
    scope
    =
    let { Load.O.finished
        ; new_rd
        ; error
        ; memory_controller_to_hart
        ; hart_to_memory_controller
        }
      =
      Load.hierarchical
        ~instance:"load"
        scope
        { Load.I.clock
        ; clear
        ; enable =
            (* We need to guard the Load instruction since it's internal
               state machine might try to load data and get stuck otherwise. *)
            enables.is_load
        ; funct3 = decoded_instruction.funct3
        ; address = decoded_instruction.load_address
        ; memory_controller_to_hart
        ; hart_to_memory_controller
        }
    in
    { Opcode_output.transaction =
        { Transaction.finished; set_rd = vdd; new_rd; error; new_pc = registers.pc +:. 4 }
    ; memory_controller_to_hart
    ; hart_to_memory_controller
    }
  ;;

  (** The store table loads a value from [rs1] and writes it to address rd *)
  let store_instruction
    ~clock
    ~clear
    ~memory_controller_to_hart
    ~hart_to_memory_controller
    ~(registers : _ Registers.t)
    ~(enables : _ Enables.t)
    (decoded_instruction : _ Decoded_instruction.t)
    scope
    =
    let ( -- ) = Scope.naming scope in
    let { Store.O.finished; error; memory_controller_to_hart; hart_to_memory_controller } =
      Store.hierarchical
        ~instance:"store"
        scope
        { Store.I.clock
        ; clear
        ; enable =
            (* We need to guard the Store instruction since it's internal
               state machine might try to load data and get stuck otherwise. *)
            enables.is_store
        ; funct3 = decoded_instruction.funct3
        ; destination = decoded_instruction.store_address
        ; value = decoded_instruction.rs2
        ; memory_controller_to_hart
        ; hart_to_memory_controller
        }
    in
    { Opcode_output.transaction =
        { Transaction.finished
        ; set_rd = gnd
        ; new_rd = zero register_width
        ; error = error -- "store_error"
        ; new_pc = registers.pc +:. 4
        }
    ; memory_controller_to_hart
    ; hart_to_memory_controller
    }
  ;;

  (** The system instruction allows access to hardware registers and ecall /
      ebreak. For ecall, the behaviour is delegated back to the user design
      via the is_ecall and ecall_transaction signals. *)
  let system_instruction
    ~clock:_
    ~clear:_
    ~memory_controller_to_hart:_
    ~hart_to_memory_controller:_
    ~(registers : _ Registers.t)
    ~ecall_transaction
    ~(enables : _ Enables.t)
    (_decoded_instruction : _ Decoded_instruction.t)
    scope
    =
    let ( -- ) = Scope.naming scope in
    (* TODO: Support hardware registers *)
    let unsupported_increment_pc =
      { Transaction.finished = vdd
      ; set_rd = vdd
      ; new_rd = one 32
      ; error = vdd
      ; new_pc = registers.pc +:. 4
      }
    in
    { Opcode_output.transaction =
        Transaction.Of_signal.mux2
          (enables.is_ecall -- "is_ecall")
          ecall_transaction
          unsupported_increment_pc
    ; memory_controller_to_hart = Memory.Rx_bus.Rx.Of_signal.of_int 0
    ; hart_to_memory_controller = Memory.Tx_bus.Tx.Of_signal.of_int 0
    }
  ;;

  module Table_entry = struct
    (* This doesn't need to be a whole type *)
    type 'a t =
      { opcode : int
      ; finished : 'a
      ; new_pc : 'a
      ; set_rd : 'a
      ; new_rd : 'a
      ; error : 'a
      ; memory_controller_to_hart : Memory.Rx_bus.Rx.Of_signal.t
      ; hart_to_memory_controller : Memory.Tx_bus.Tx.Of_signal.t
      }

    let create
      ~opcode
      { Opcode_output.transaction =
          { Transaction.finished; new_pc; set_rd; new_rd; error }
      ; memory_controller_to_hart
      ; hart_to_memory_controller
      }
      =
      { finished
      ; opcode
      ; new_pc
      ; set_rd
      ; new_rd
      ; error
      ; memory_controller_to_hart
      ; hart_to_memory_controller
      }
    ;;
  end

  let instruction_table
    ~clock
    ~clear
    ~memory_controller_to_hart
    ~hart_to_memory_controller
    ~registers
    ~decoded_instruction
    ~enables
    ~ecall_transaction
    scope
    =
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
        (jalr_instruction ~registers decoded_instruction scope)
    ; Table_entry.create
        ~opcode:Opcodes.lui
        (lui_instruction ~registers decoded_instruction)
    ; Table_entry.create
        ~opcode:Opcodes.auipc
        (auipc_instruction ~registers decoded_instruction)
    ; Table_entry.create ~opcode:Opcodes.fence (fence ~registers decoded_instruction)
    ; Table_entry.create
        ~opcode:Opcodes.branch
        (branch_instruction ~registers decoded_instruction scope)
    ; Table_entry.create
        ~opcode:Opcodes.load
        (load_instruction
           ~clock
           ~clear
           ~memory_controller_to_hart
           ~hart_to_memory_controller
           ~registers
           ~enables
           decoded_instruction
           scope)
    ; Table_entry.create
        ~opcode:Opcodes.store
        (store_instruction
           ~clock
           ~clear
           ~memory_controller_to_hart
           ~hart_to_memory_controller
           ~registers
           ~enables
           decoded_instruction
           scope)
    ; Table_entry.create
        ~opcode:Opcodes.system
        (system_instruction
           ~clock
           ~clear
           ~memory_controller_to_hart
           ~hart_to_memory_controller
           ~registers
           ~ecall_transaction
           ~enables
           decoded_instruction
           scope)
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
    let ( -- ) = Scope.naming scope in
    let reg_spec = Reg_spec.create ~clear:i.clear ~clock:i.clock () in
    let reg_spec_no_clear = Reg_spec.create ~clock:i.clock () in
    let memory_controller_to_hart = Memory.Rx_bus.Rx.Of_always.wire zero in
    let hart_to_memory_controller = Memory.Tx_bus.Tx.Of_always.wire zero in
    let new_registers = Registers.For_writeback.Of_always.wire zero in
    let decoded_instruction = Decoded_instruction.Of_always.reg reg_spec_no_clear in
    let enables = Enables.Of_always.reg reg_spec_no_clear in
    let transaction = Transaction.Of_always.reg reg_spec_no_clear in
    (* TODO: Staging the muxes into and out of registers might make this slightly cheaper *)
    let current_state = State_machine.create (module State) reg_spec in
    ignore (current_state.current -- "current_state" : Signal.t);
    let commit_transaction ~new_pc ~set_rd ~new_rd =
      Registers.set_pc i.registers new_pc
      |> Registers.assign_when
           ~when_:set_rd
           ~index_signal:(decoded_instruction.rd.value -- "t$rd")
           ~value_signal:new_rd
    in
    let instruction_table =
      instruction_table
        ~clock:i.clock
        ~clear:i.clear
        ~memory_controller_to_hart:i.memory_controller_to_hart
        ~hart_to_memory_controller:i.hart_to_memory_controller
        ~decoded_instruction:(Decoded_instruction.Of_always.value decoded_instruction)
        ~enables:(Enables.Of_always.value enables)
        ~registers:i.registers
        ~ecall_transaction:i.ecall_transaction
        scope
    in
    let decoded_opcode = decoded_instruction.opcode.value -- "opcode" in
    compile
      [ current_state.switch
          [ ( State.Decoding
            , [ when_
                  i.process_instruction
                  [ Decoded_instruction.Of_always.assign
                      decoded_instruction
                      (Decoded_instruction.of_instruction i.instruction i.registers scope)
                  ; Enables.Of_always.assign
                      enables
                      (Enables.of_instruction i.instruction i.registers scope)
                  ; current_state.set_next Executing
                  ]
              ] )
          ; ( State.Executing
            , List.map
                ~f:
                  (fun
                    { Table_entry.finished
                    ; opcode
                    ; new_pc
                    ; set_rd
                    ; new_rd
                    ; error
                    ; hart_to_memory_controller = op_to_mem_ctrl
                    ; memory_controller_to_hart = mem_ctrl_to_op
                    }
                  ->
                  when_
                    (decoded_opcode ==:. opcode)
                    [ Memory.Rx_bus.Rx.Of_always.assign
                        memory_controller_to_hart
                        mem_ctrl_to_op
                    ; Memory.Tx_bus.Tx.Of_always.assign
                        hart_to_memory_controller
                        op_to_mem_ctrl
                    ; when_
                        finished
                        [ Transaction.Of_always.assign
                            transaction
                            { finished = vdd; new_pc; set_rd; new_rd; error }
                        ; Enables.Of_always.assign enables (Enables.Of_signal.of_int 0)
                        ; current_state.set_next Committing
                        ]
                    ])
                instruction_table )
            (* TODO: Trap when no opcode matches *)
          ; ( Committing
            , [ Registers.For_writeback.Of_always.assign
                  new_registers
                  (commit_transaction
                     ~new_pc:(transaction.new_pc.value -- "t$new_pc")
                     ~set_rd:(transaction.set_rd.value -- "t$set_rd")
                     ~new_rd:(transaction.new_rd.value -- "t$new_rd")
                   |> Registers.For_writeback.of_registers)
              ; Decoded_instruction.Of_always.assign
                  decoded_instruction
                  (Decoded_instruction.Of_signal.of_int 0)
              ; current_state.set_next Decoding
              ] )
          ]
      ];
    { O.memory_controller_to_hart =
        Memory.Rx_bus.Rx.Of_always.value memory_controller_to_hart
    ; hart_to_memory_controller =
        Memory.Tx_bus.Tx.Of_always.value hart_to_memory_controller
    ; finished = current_state.is Committing
    ; new_registers = Registers.For_writeback.Of_always.value new_registers
    ; error = transaction.error.value
    ; is_ecall = enables.is_ecall.value
    }
  ;;

  let hierarchical ~instance (scope : Scope.t) (input : Signal.t I.t) =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ~scope ~name:"decode_and_execute" ~instance create input
  ;;
end
