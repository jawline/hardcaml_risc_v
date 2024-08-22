open! Core
open Hardcaml
open Hardcaml_memory_controller
open Signal

module Make
    (Hart_config : Hart_config_intf.S)
    (Memory : Memory_bus_intf.S)
    (Registers : Registers_intf.S)
    (Decoded_instruction : Decoded_instruction_intf.M(Registers).S)
    (Transaction : Transaction_intf.S) =
struct
  module Op = Op.Make (Hart_config)
  module Branch = Branch.Make (Hart_config)
  module Load = Load.Make (Hart_config) (Memory)
  module Store = Store.Make (Hart_config) (Memory)

  let register_width = Register_width.bits Hart_config.register_width

  module Opcode_output = struct
    type 'a t =
      { valid : 'a
      ; transaction : 'a Transaction.t
      ; hart_to_memory_controller : 'a Memory.Tx_bus.Tx.t option
      }
  end

  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; valid : 'a
      ; registers : 'a Registers.For_writeback.t
      ; instruction : 'a Decoded_instruction.t
      ; memory_controller_to_hart : 'a Memory.Rx_bus.Tx.t
      ; hart_to_memory_controller : 'a Memory.Tx_bus.Rx.t
      ; ecall_transaction : 'a Transaction.t
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t =
      { valid : 'a
      ; registers : 'a Registers.For_writeback.t
      ; instruction : 'a Decoded_instruction.t
      ; transaction : 'a Transaction.t
      ; hart_to_memory_controller : 'a Memory.Tx_bus.Tx.t
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
      Op.hierarchical (* There is no SUBI since a signed addi is sufficient. *)
        ~enable_subtract:false
        ~instance:"op_imm"
        scope
        { Op.I.funct3; funct7; lhs = rs1; rhs = i_immediate }
    in
    { Opcode_output.valid = vdd
    ; hart_to_memory_controller = None
    ; transaction =
        { Transaction.set_rd = vdd; new_rd; error; new_pc = registers.pc +:. 4 }
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
    { Opcode_output.valid = vdd
    ; hart_to_memory_controller = None
    ; transaction =
        { Transaction.set_rd = vdd; new_rd; error; new_pc = registers.pc +:. 4 }
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
    { Opcode_output.valid = vdd
    ; hart_to_memory_controller = None
    ; transaction =
        { Transaction.set_rd = vdd; new_rd = registers.pc +:. 4; new_pc; error }
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
    let new_pc =
      let%hw jalr_rs1 = decoded_instruction.rs1 in
      let%hw jalr_i = decoded_instruction.i_immediate in
      jalr_rs1 +: jalr_i &: ~:(of_int ~width:register_width 1)
    in
    let error = new_pc &:. 0b11 <>:. 0 in
    { Opcode_output.valid = vdd
    ; hart_to_memory_controller = None
    ; transaction =
        { Transaction.set_rd = vdd; new_pc; error; new_rd = registers.pc +:. 4 }
    }
  ;;

  (** LUI (load upper immediate) sets rd to the decoded U immediate (20 bit
      value from the msb with zeros for the lower 12. *)
  let lui_instruction
    ~(registers : _ Registers.t)
    (decoded_instruction : _ Decoded_instruction.t)
    =
    { Opcode_output.valid = vdd
    ; hart_to_memory_controller = None
    ; transaction =
        { Transaction.set_rd = vdd
        ; new_rd = decoded_instruction.u_immediate
        ; error = gnd
        ; new_pc = registers.pc +:. 4
        }
    }
  ;;

  (** Add upper immediate to PC. Similar to LUI but adds the loaded immediate to
      current the program counter and places it in RD. This can be used to compute
      addresses for JALR instructions. *)
  let auipc_instruction
    ~(registers : _ Registers.t)
    (decoded_instruction : _ Decoded_instruction.t)
    =
    { Opcode_output.valid = vdd
    ; hart_to_memory_controller = None
    ; transaction =
        { Transaction.set_rd = vdd
        ; new_rd = registers.pc +:. 4
        ; error = gnd
        ; new_pc = registers.pc +: decoded_instruction.u_immediate
        }
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
    { Opcode_output.valid = vdd
    ; hart_to_memory_controller = None
    ; transaction =
        { Transaction.set_rd = gnd; new_rd = zero register_width; error; new_pc }
    }
  ;;

  let fence ~(registers : _ Registers.t) (_decoded_instruction : _ Decoded_instruction.t) =
    (* TODO: Currently all memory transactions are atomic so I'm not sure if I
     * need to implement this. Figure it out. *)
    { Opcode_output.valid = vdd
    ; hart_to_memory_controller = None
    ; transaction =
        { Transaction.set_rd = vdd
        ; new_rd = zero register_width
        ; error = gnd
        ; new_pc = registers.pc +:. 4
        }
    }
  ;;

  (** The load table loads a value from [rs1] and places it in rd *)
  let load_instruction
    ~clock
    ~clear
    ~memory_controller_to_hart
    ~hart_to_memory_controller
    ~(registers : _ Registers.t)
    (decoded_instruction : _ Decoded_instruction.t)
    scope
    =
    let { Load.O.finished; new_rd; error; hart_to_memory_controller } =
      Load.hierarchical
        ~instance:"load"
        scope
        { Load.I.clock
        ; clear
        ; enable =
            (* We need to guard the Load instruction since it's internal
               state machine might try to load data and get stuck otherwise. *)
            decoded_instruction.is_load
        ; funct3 = decoded_instruction.funct3
        ; address = decoded_instruction.load_address
        ; memory_controller_to_hart
        ; hart_to_memory_controller
        }
    in
    { Opcode_output.valid = finished
    ; transaction =
        { Transaction.set_rd = vdd; new_rd; error; new_pc = registers.pc +:. 4 }
    ; hart_to_memory_controller = Some hart_to_memory_controller
    }
  ;;

  (** The store table loads a value from [rs1] and writes it to address rd *)
  let store_instruction
    ~clock
    ~clear
    ~memory_controller_to_hart
    ~hart_to_memory_controller
    ~(registers : _ Registers.t)
    (decoded_instruction : _ Decoded_instruction.t)
    scope
    =
    let { Store.O.finished; error; hart_to_memory_controller } =
      Store.hierarchical
        ~instance:"store"
        scope
        { Store.I.clock
        ; clear
        ; enable =
            (* We need to guard the Store instruction since it's internal
               state machine might try to load data and get stuck otherwise. *)
            decoded_instruction.is_store
        ; funct3 = decoded_instruction.funct3
        ; destination = decoded_instruction.store_address
        ; value = decoded_instruction.rs2
        ; memory_controller_to_hart
        ; hart_to_memory_controller
        }
    in
    { Opcode_output.valid = finished
    ; transaction =
        { Transaction.set_rd = gnd
        ; new_rd = zero register_width
        ; error
        ; new_pc = registers.pc +:. 4
        }
    ; hart_to_memory_controller = Some hart_to_memory_controller
    }
  ;;

  (** The system instruction allows access to hardware registers and ecall /
      ebreak. For ecall, the behaviour is delegated back to the user design
      via the is_ecall and ecall_transaction signals. *)
  let system_instruction
    ~clock:_
    ~clear:_
    ~(registers : _ Registers.t)
    ~ecall_transaction
    (decoded_instruction : _ Decoded_instruction.t)
    scope
    =
    (* TODO: Support hardware registers *)
    let unsupported_increment_pc =
      { Transaction.set_rd = vdd
      ; new_rd = one 32
      ; error = vdd
      ; new_pc = registers.pc +:. 4
      }
    in
    let%hw is_ecall = decoded_instruction.is_ecall in
    { Opcode_output.valid = vdd
    ; hart_to_memory_controller = None
    ; transaction =
        Transaction.Of_signal.mux2 is_ecall ecall_transaction unsupported_increment_pc
    }
  ;;

  module Table_entry = struct
    (* This doesn't need to be a whole type *)
    type 'a t =
      { opcode : int
      ; output : 'a Opcode_output.t
      }

    let create ~opcode output = { opcode; output }
  end

  let instruction_table
    ~clock
    ~clear
    ~registers
    ~decoded_instruction
    ~ecall_transaction
    ~memory_controller_to_hart
    ~hart_to_memory_controller
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
           decoded_instruction
           scope)
    ; Table_entry.create
        ~opcode:Opcodes.system
        (system_instruction
           ~clock
           ~clear
           ~registers
           ~ecall_transaction
           decoded_instruction
           scope)
    ]
  ;;

  let create scope (i : _ I.t) =
    let reg_spec_with_clear = Reg_spec.create ~clock:i.clock ~clear:i.clear () in
    let instruction_table =
      instruction_table
        ~clock:i.clock
        ~clear:i.clear
        ~decoded_instruction:i.instruction
        ~registers:(Registers.For_writeback.to_registers i.registers)
        ~memory_controller_to_hart:i.memory_controller_to_hart
        ~hart_to_memory_controller:i.hart_to_memory_controller
        ~ecall_transaction:(assert false)
        scope
    in
    let transaction =
      let instruction_mux =
        List.init
          ~f:(fun opcode ->
            match List.find ~f:(fun t -> t.opcode = opcode) instruction_table with
            | Some t -> t.output.transaction
            | None -> Transaction.generic_error)
          128
      in
      Transaction.Of_signal.reg
        reg_spec_with_clear
        (Transaction.Of_signal.mux i.instruction.opcode instruction_mux)
    in
    { O.valid = reg reg_spec_with_clear i.valid
    ; registers = Registers.For_writeback.Of_signal.reg reg_spec_with_clear i.registers
    ; instruction = Decoded_instruction.Of_signal.reg reg_spec_with_clear i.instruction
    ; transaction
    ; hart_to_memory_controller =
        (let combine = Memory.Tx_bus.Tx.map2 ~f:( |: ) in
         List.map ~f:(fun t -> t.output.hart_to_memory_controller) instruction_table
         |> List.filter_opt
         |> List.fold ~init:(Memory.Tx_bus.Tx.Of_signal.of_int 0) ~f:combine)
    ; error = transaction.error
    }
  ;;

  let hierarchical (scope : Scope.t) (input : Signal.t I.t) =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ~scope ~name:"execute" create input
  ;;
end
