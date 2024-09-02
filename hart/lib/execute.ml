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
      ; write_bus : 'a Memory.Write_bus.Tx.t option
      ; read_bus : 'a Memory.Read_bus.Tx.t option
      }
  end

  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; valid : 'a
      ; registers : 'a Registers.For_writeback.t [@rtlprefix "registers$"]
      ; instruction : 'a Decoded_instruction.t
      ; ecall_transaction : 'a Transaction.t
      ; error : 'a
      ; write_bus : 'a Memory.Write_bus.Rx.t [@rtlprefix "write$"]
      ; read_bus : 'a Memory.Read_bus.Rx.t [@rtlprefix "read$"]
      ; write_response : 'a Memory.Write_response.With_valid.t
           [@rtlprefix "write_response$"]
      ; read_response : 'a Memory.Read_response.With_valid.t [@rtlprefix "read_response$"]
      }
    [@@deriving sexp_of, hardcaml ~rtlmangle:"$"]
  end

  module O = struct
    type 'a t =
      { valid : 'a
      ; registers : 'a Registers.For_writeback.t [@rtlprefix "registers$"]
      ; instruction : 'a Decoded_instruction.t
      ; transaction : 'a Transaction.t
      ; error : 'a
      ; is_ecall : 'a
      ; write_bus : 'a Memory.Write_bus.Tx.t [@rtlprefix "write$"]
      ; read_bus : 'a Memory.Read_bus.Tx.t [@rtlprefix "read$"]
      }
    [@@deriving sexp_of, hardcaml ~rtlmangle:"$"]
  end

  let op_imm_instructions
    ~valid
    ~(registers : _ Registers.t)
    scope
    ({ funct3
     ; rs1
     ; i_immediate
     ; funct7_switch
     ; funct7_bit_other_than_switch_is_selected
     ; _
     } :
      _ Decoded_instruction.t)
    =
    let { Op.O.rd = new_rd; error } =
      Op.hierarchical (* There is no SUBI since a signed addi is sufficient. *)
        ~enable_subtract:false
        ~instance:"op_imm"
        scope
        { Op.I.funct3
        ; funct7_switch
        ; funct7_error = funct7_bit_other_than_switch_is_selected
        ; lhs = rs1
        ; rhs = i_immediate
        }
    in
    { Opcode_output.valid
    ; read_bus = None
    ; write_bus = None
    ; transaction =
        { Transaction.set_rd = vdd; new_rd; error; new_pc = registers.pc +:. 4 }
    }
  ;;

  let op_instructions
    ~valid
    ~(registers : _ Registers.t)
    scope
    ({ funct3; rs1; rs2; funct7_switch; funct7_bit_other_than_switch_is_selected; _ } :
      _ Decoded_instruction.t)
    =
    let { Op.O.rd = new_rd; error } =
      Op.hierarchical
        ~instance:"op"
        ~enable_subtract:true
        scope
        { Op.I.funct3
        ; funct7_switch
        ; funct7_error = funct7_bit_other_than_switch_is_selected
        ; lhs = rs1
        ; rhs = rs2
        }
    in
    { Opcode_output.valid
    ; read_bus = None
    ; write_bus = None
    ; transaction =
        { Transaction.set_rd = vdd; new_rd; error; new_pc = registers.pc +:. 4 }
    }
  ;;

  (** JAL (jump and link) adds the signed J-immediate value to the current PC
      after storing the current PC + 4 in the destination register. *)
  let jal_instruction
    ~valid
    ~(registers : _ Registers.t)
    (decoded_instruction : _ Decoded_instruction.t)
    scope
    =
    let%hw new_pc = registers.pc +: decoded_instruction.j_immediate in
    { Opcode_output.valid
    ; read_bus = None
    ; write_bus = None
    ; transaction =
        { Transaction.set_rd = vdd; new_rd = registers.pc +:. 4; new_pc; error = gnd }
    }
  ;;

  let drop_lsb t = concat_msb [ sel_top ~width:(width t - 1) t; gnd ]

  (** JALR (Indirect jump) adds a 12-bit signed immediate to whatever is at rs1,
      sets the LSB of that result to zero (e.g, result = result & (!1)), and
      finally sets the PC to this new result.  rd is set to the original PC + 4
      (the start of the next instruction).  Regiser 0 can be used to discard the
      result. *)
  let jalr_instruction
    ~valid
    ~(registers : _ Registers.t)
    (decoded_instruction : _ Decoded_instruction.t)
    scope
    =
    let new_pc =
      let%hw jalr_rs1 = decoded_instruction.rs1 in
      let%hw jalr_i = decoded_instruction.i_immediate in
      jalr_rs1 +: jalr_i |> drop_lsb
    in
    { Opcode_output.valid
    ; read_bus = None
    ; write_bus = None
    ; transaction =
        { Transaction.set_rd = vdd; new_pc; error = gnd; new_rd = registers.pc +:. 4 }
    }
  ;;

  (** LUI (load upper immediate) sets rd to the decoded U immediate (20 bit
      value from the msb with zeros for the lower 12. *)
  let lui_instruction
    ~valid
    ~(registers : _ Registers.t)
    (decoded_instruction : _ Decoded_instruction.t)
    =
    { Opcode_output.valid
    ; read_bus = None
    ; write_bus = None
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
    ~valid
    ~(registers : _ Registers.t)
    (decoded_instruction : _ Decoded_instruction.t)
    =
    { Opcode_output.valid
    ; read_bus = None
    ; write_bus = None
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
    ~valid
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
    { Opcode_output.valid
    ; read_bus = None
    ; write_bus = None
    ; transaction =
        { Transaction.set_rd = gnd; new_rd = zero register_width; error; new_pc }
    }
  ;;

  let fence
    ~valid
    ~(registers : _ Registers.t)
    (_decoded_instruction : _ Decoded_instruction.t)
    =
    (* TODO: Currently all memory transactions are atomic so I'm not sure if I
     * need to implement this. Figure it out. *)
    { Opcode_output.valid
    ; read_bus = None
    ; write_bus = None
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
    ~valid
    ~read_bus
    ~read_response
    ~(registers : _ Registers.t)
    (decoded_instruction : _ Decoded_instruction.t)
    scope
    =
    let { Load.O.finished; new_rd; error; read_bus } =
      Load.hierarchical
        ~instance:"load"
        scope
        { Load.I.clock
        ; clear
        ; enable =
            (* We need to guard the Load instruction since it's internal
               state machine might try to load data and get stuck otherwise. *)
            valid &: decoded_instruction.is_load
        ; funct3 = decoded_instruction.funct3
        ; address = decoded_instruction.load_address
        ; read_bus
        ; read_response
        }
    in
    { Opcode_output.valid = finished
    ; read_bus = Some read_bus
    ; write_bus = None
    ; transaction =
        { Transaction.set_rd = vdd; new_rd; error; new_pc = registers.pc +:. 4 }
    }
  ;;

  (** The store table loads a value from [rs1] and writes it to address rd *)
  let store_instruction
    ~clock
    ~clear
    ~valid
    ~read_bus
    ~read_response
    ~write_bus
    ~write_response
    ~(registers : _ Registers.t)
    (decoded_instruction : _ Decoded_instruction.t)
    scope
    =
    let { Store.O.finished; error; read_bus; write_bus } =
      Store.hierarchical
        ~instance:"store"
        scope
        { Store.I.clock
        ; clear
        ; enable =
            (* We need to guard the Store instruction since it's internal
               state machine might try to load data and get stuck otherwise. *)
            valid &: decoded_instruction.is_store
        ; funct3 = decoded_instruction.funct3
        ; destination = decoded_instruction.store_address
        ; value = decoded_instruction.rs2
        ; read_bus
        ; read_response
        ; write_bus
        ; write_response
        }
    in
    { Opcode_output.valid = finished
    ; transaction =
        { Transaction.set_rd = gnd
        ; new_rd = zero register_width
        ; error
        ; new_pc = registers.pc +:. 4
        }
    ; read_bus = Some read_bus
    ; write_bus = Some write_bus
    }
  ;;

  (** The system instruction allows access to hardware registers and ecall /
      ebreak. For ecall, the behaviour is delegated back to the user design
      via the is_ecall and ecall_transaction signals. *)
  let system_instruction
    ~clock:_
    ~clear:_
    ~valid
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
    { Opcode_output.valid
    ; write_bus = None
    ; read_bus = None
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
    ~valid
    ~registers
    ~decoded_instruction
    ~ecall_transaction
    ~read_bus
    ~read_response
    ~write_bus
    ~write_response
    scope
    =
    [ Table_entry.create
        ~opcode:Opcodes.op
        (op_instructions ~valid ~registers scope decoded_instruction)
    ; Table_entry.create
        ~opcode:Opcodes.op_imm
        (op_imm_instructions ~valid ~registers scope decoded_instruction)
    ; Table_entry.create
        ~opcode:Opcodes.jal
        (jal_instruction
           ~valid
           ~registers
           decoded_instruction
           (Scope.sub_scope scope "jal"))
    ; Table_entry.create
        ~opcode:Opcodes.jalr
        (jalr_instruction ~valid ~registers decoded_instruction scope)
    ; Table_entry.create
        ~opcode:Opcodes.lui
        (lui_instruction ~valid ~registers decoded_instruction)
    ; Table_entry.create
        ~opcode:Opcodes.auipc
        (auipc_instruction ~valid ~registers decoded_instruction)
    ; Table_entry.create
        ~opcode:Opcodes.fence
        (fence ~valid ~registers decoded_instruction)
    ; Table_entry.create
        ~opcode:Opcodes.branch
        (branch_instruction ~valid ~registers decoded_instruction scope)
    ; Table_entry.create
        ~opcode:Opcodes.load
        (load_instruction
           ~clock
           ~clear
           ~valid
           ~read_bus
           ~read_response
           ~registers
           decoded_instruction
           scope)
    ; Table_entry.create
        ~opcode:Opcodes.store
        (store_instruction
           ~clock
           ~clear
           ~valid
           ~read_bus
           ~read_response
           ~write_bus
           ~write_response
           ~registers
           decoded_instruction
           scope)
    ; Table_entry.create
        ~opcode:Opcodes.system
        (system_instruction
           ~clock
           ~clear
           ~valid
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
        ~valid:i.valid
        ~decoded_instruction:i.instruction
        ~registers:(Registers.For_writeback.to_registers i.registers)
        ~read_bus:i.read_bus
        ~read_response:i.read_response
        ~write_bus:i.write_bus
        ~write_response:i.write_response
        ~ecall_transaction:i.ecall_transaction
        scope
    in
    let valid =
      let valid_mux =
        List.init
          ~f:(fun opcode ->
            match List.find ~f:(fun t -> t.opcode = opcode) instruction_table with
            | Some t -> t.output.valid
            | None -> gnd)
          128
      in
      reg reg_spec_with_clear (mux i.instruction.opcode valid_mux)
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
    { O.valid
    ; registers =
        Registers.For_writeback.Of_signal.reg
          ~enable:i.valid
          reg_spec_with_clear
          i.registers
    ; instruction =
        Decoded_instruction.Of_signal.reg
          ~enable:i.valid
          reg_spec_with_clear
          i.instruction
    ; transaction
    ; read_bus =
        (let combine = Memory.Read_bus.Tx.map2 ~f:( |: ) in
         let gate (t : _ Memory.Read_bus.Tx.t) =
           Memory.Read_bus.Tx.Of_signal.mux2
             t.valid
             t
             (Memory.Read_bus.Tx.Of_signal.of_int 0)
         in
         List.filter_map ~f:(fun t -> t.output.read_bus) instruction_table
         |> List.map ~f:gate
         |> List.fold ~init:(Memory.Read_bus.Tx.Of_signal.of_int 0) ~f:combine)
    ; write_bus =
        (let combine = Memory.Write_bus.Tx.map2 ~f:( |: ) in
         let gate (t : _ Memory.Write_bus.Tx.t) =
           Memory.Write_bus.Tx.Of_signal.mux2
             t.valid
             t
             (Memory.Write_bus.Tx.Of_signal.of_int 0)
         in
         List.filter_map ~f:(fun t -> t.output.write_bus) instruction_table
         |> List.map ~f:gate
         |> List.fold ~init:(Memory.Write_bus.Tx.Of_signal.of_int 0) ~f:combine)
    ; error = reg ~enable:i.error reg_spec_with_clear i.error |: transaction.error
    ; is_ecall = i.instruction.is_ecall
    }
  ;;

  let hierarchical (scope : Scope.t) (input : Signal.t I.t) =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ~scope ~name:"execute" create input
  ;;
end
