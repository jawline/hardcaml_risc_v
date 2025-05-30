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
  module Csr = Csr.Make (Hart_config) (Registers) (Decoded_instruction)
  module Op = Op.Make (Hart_config)
  module Branch = Branch.Make (Hart_config)
  module Load = Load.Make (Hart_config) (Memory)
  module Store = Store.Make (Hart_config) (Memory)

  let register_width = Register_width.bits Hart_config.register_width

  module Opcode_output = struct
    type 'a t =
      { valid : 'a
      ; transaction : 'a Transaction.t
      ; write_bus : 'a Memory.Write_bus.Source.t option
      ; read_bus : 'a Memory.Read_bus.Source.t option
      }
    [@@deriving fields ~getters]
  end

  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; valid : 'a [@rtlname "input_valid"]
      ; registers : 'a Registers.For_writeback.t
      ; instruction : 'a Decoded_instruction.t
      ; ecall_transaction : 'a Transaction.With_valid.t
      ; error : 'a [@rtlname "input_error"]
      ; write_bus : 'a Memory.Write_bus.Dest.t
      ; read_bus : 'a Memory.Read_bus.Dest.t
      ; write_response : 'a Memory.Write_response.With_valid.t
      ; read_response : 'a Memory.Read_response.With_valid.t
      ; instret : 'a
      }
    [@@deriving hardcaml ~rtlmangle:"$input_"]
  end

  module O = struct
    type 'a t =
      { valid : 'a [@rtlname "output_valid"]
      ; registers : 'a Registers.For_writeback.t
      ; instruction : 'a Decoded_instruction.t
      ; transaction : 'a Transaction.t
      ; error : 'a [@rtlname "output_error"]
      ; is_ecall : 'a
      ; write_bus : 'a Memory.Write_bus.Source.t
      ; read_bus : 'a Memory.Read_bus.Source.t
      }
    [@@deriving hardcaml ~rtlmangle:"$output_"]
  end

  let op_instructions
        ~valid
        ~(registers : _ Registers.t)
        scope
        ({ opcode; argument_1; argument_2; alu_specifics; op_onehot; _ } :
          _ Decoded_instruction.t)
    =
    let { Op.O.rd = new_rd } =
      Op.hierarchical
        ~instance:"op"
        scope
        { Op.I.funct3 = op_onehot
        ; subtract_instead_of_add = alu_specifics.subtract_instead_of_add
        ; arithmetic_shift = alu_specifics.arithmetic_shift
        ; lhs = argument_1
        ; rhs = argument_2
        }
    in
    { Opcode_output.valid = valid &: Decoded_opcode.valid opcode ALU
    ; read_bus = None
    ; write_bus = None
    ; transaction =
        { Transaction.set_rd = vdd; new_rd; error = gnd; new_pc = registers.pc +:. 4 }
    }
  ;;

  (** Assign PC = argument 1 + argument 2, set rd = pc + 4. Implements JAL and JALR. *)
  let assign_pc_sum_of_arguments
        ~valid
        ~(registers : _ Registers.t)
        (decoded_instruction : _ Decoded_instruction.t)
        scope
    =
    let new_pc =
      let%hw arg1 = decoded_instruction.argument_1 in
      let%hw arg2 = decoded_instruction.argument_2 in
      arg1 +: arg2
    in
    let new_pc = concat_msb [ drop_bottom ~width:1 new_pc; zero 1 ] in
    { Opcode_output.valid =
        valid
        &: Decoded_opcode.valid decoded_instruction.opcode Assign_pc_sum_of_arguments
    ; read_bus = None
    ; write_bus = None
    ; transaction =
        { Transaction.set_rd = vdd; new_rd = registers.pc +:. 4; new_pc; error = gnd }
    }
  ;;

  (** LUI (load upper immediate) sets rd to the decoded U immediate (20 bit
      value from the msb with zeros for the lower 12. *)
  let lui_instruction
        ~valid
        ~(registers : _ Registers.t)
        (decoded_instruction : _ Decoded_instruction.t)
    =
    { Opcode_output.valid = valid &: Decoded_opcode.valid decoded_instruction.opcode Lui
    ; read_bus = None
    ; write_bus = None
    ; transaction =
        { Transaction.set_rd = vdd
        ; new_rd = decoded_instruction.argument_1
        ; error = gnd
        ; new_pc = registers.pc +:. 4
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
    let { Branch.O.new_pc } =
      Branch.hierarchical
        ~instance:"branch"
        scope
        { Branch.I.funct3 = decoded_instruction.branch_onehot
        ; lhs = decoded_instruction.argument_1
        ; rhs = decoded_instruction.argument_2
        ; branch_offset = decoded_instruction.argument_3
        ; pc = registers.pc
        }
    in
    { Opcode_output.valid =
        valid &: Decoded_opcode.valid decoded_instruction.opcode Branch
    ; read_bus = None
    ; write_bus = None
    ; transaction =
        { Transaction.set_rd = gnd; new_rd = zero register_width; error = gnd; new_pc }
    }
  ;;

  let fence
        ~valid
        ~(registers : _ Registers.t)
        (decoded_instruction : _ Decoded_instruction.t)
    =
    { Opcode_output.valid = valid &: Decoded_opcode.valid decoded_instruction.opcode Fence
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
            valid &: Decoded_opcode.valid decoded_instruction.opcode Load
        ; funct3 = decoded_instruction.funct3
        ; address = decoded_instruction.argument_1 +: decoded_instruction.argument_3
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
            valid &: Decoded_opcode.valid decoded_instruction.opcode Store
        ; funct3 = decoded_instruction.funct3
        ; destination = decoded_instruction.argument_1 +: decoded_instruction.argument_3
        ; value = decoded_instruction.argument_2
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
        ~clock
        ~clear
        ~valid
        ~instret
        ~(registers : _ Registers.t)
        ~(ecall_transaction : _ Transaction.With_valid.t)
        (decoded_instruction : _ Decoded_instruction.t)
        scope
    =
    let transaction_of t =
      { Transaction.set_rd = vdd; new_rd = t; error = gnd; new_pc = registers.pc +:. 4 }
    in
    let%hw is_ecall =
      Decoded_opcode.valid decoded_instruction.opcode System
      &: decoded_instruction.is_ecall
    in
    let csr =
      Csr.hierarchical
        scope
        { Csr.I.clock; clear; valid; instruction = decoded_instruction; instret }
    in
    let ecall_valid = valid &: is_ecall in
    let csr_valid = csr.valid in
    { Opcode_output.valid = ecall_valid |: csr_valid
    ; write_bus = None
    ; read_bus = None
    ; transaction =
        Transaction.Of_signal.onehot_select
          [ { With_valid.valid = valid &: is_ecall; value = ecall_transaction.value }
          ; { With_valid.valid = csr.valid; value = transaction_of csr.value }
          ]
    }
  ;;

  module Table_entry = struct
    (* This doesn't need to be a whole type *)
    type 'a t =
      { opcode : Decoded_opcode.t
      ; output : 'a Opcode_output.t
      }
    [@@deriving fields ~getters]

    let create ~opcode output = { opcode; output }
  end

  let instruction_table
        ~clock
        ~clear
        ~valid
        ~instret
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
        ~opcode:Decoded_opcode.ALU
        (op_instructions ~valid ~registers scope decoded_instruction)
    ; Table_entry.create
        ~opcode:Decoded_opcode.Assign_pc_sum_of_arguments
        (assign_pc_sum_of_arguments ~valid ~registers decoded_instruction scope)
    ; Table_entry.create
        ~opcode:Decoded_opcode.Lui
        (lui_instruction ~valid ~registers decoded_instruction)
    ; Table_entry.create
        ~opcode:Decoded_opcode.Fence
        (fence ~valid ~registers decoded_instruction)
    ; Table_entry.create
        ~opcode:Decoded_opcode.Branch
        (branch_instruction ~valid ~registers decoded_instruction scope)
    ; Table_entry.create
        ~opcode:Decoded_opcode.Load
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
        ~opcode:Decoded_opcode.Store
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
        ~opcode:Decoded_opcode.System
        (system_instruction
           ~clock
           ~clear
           ~valid
           ~registers
           ~ecall_transaction
           ~instret
           decoded_instruction
           scope)
    ]
  ;;

  let combine_read_bus (instruction_table : _ Table_entry.t list) =
    let combine = Memory.Read_bus.Source.map2 ~f:( |: ) in
    let gate (t : _ Memory.Read_bus.Source.t) =
      Memory.Read_bus.Source.Of_signal.mux2
        t.valid
        t
        (Memory.Read_bus.Source.Of_signal.of_int 0)
    in
    List.filter_map ~f:(fun t -> t.output.read_bus) instruction_table
    |> List.map ~f:gate
    |> List.fold ~init:(Memory.Read_bus.Source.Of_signal.of_int 0) ~f:combine
  ;;

  let combine_write_bus (instruction_table : _ Table_entry.t list) =
    let combine = Memory.Write_bus.Source.map2 ~f:( |: ) in
    let gate (t : _ Memory.Write_bus.Source.t) =
      Memory.Write_bus.Source.Of_signal.mux2
        t.valid
        t
        (Memory.Write_bus.Source.Of_signal.of_int 0)
    in
    List.filter_map ~f:(fun t -> t.output.write_bus) instruction_table
    |> List.map ~f:gate
    |> List.fold ~init:(Memory.Write_bus.Source.Of_signal.of_int 0) ~f:combine
  ;;

  let create scope (i : _ I.t) =
    let reg_spec_with_clear = Reg_spec.create ~clock:i.clock ~clear:i.clear () in
    let instruction_table =
      instruction_table
        ~clock:i.clock
        ~clear:i.clear
        ~valid:i.valid
        ~instret:i.instret
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
      let valid =
        [ i.valid &: i.instruction.error ]
        @ (List.map ~f:Table_entry.output instruction_table
           |> List.map ~f:Opcode_output.valid)
        |> List.reduce_exn ~f:( |: )
      in
      reg reg_spec_with_clear valid
    in
    let transaction =
      let error_with_valid =
        { With_valid.valid = i.instruction.error; value = Transaction.generic_error }
      in
      let opcodes_with_valid =
        List.map ~f:Table_entry.output instruction_table
        |> List.map ~f:(fun output ->
          { With_valid.valid = output.valid; value = output.transaction })
      in
      let result =
        Transaction.Of_signal.onehot_select (error_with_valid :: opcodes_with_valid)
      in
      Transaction.Of_signal.reg reg_spec_with_clear result
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
    ; error = reg ~enable:i.valid reg_spec_with_clear i.error |: transaction.error
    ; is_ecall =
        i.valid
        &: Decoded_opcode.valid i.instruction.opcode System
        &: i.instruction.is_ecall
    ; read_bus = combine_read_bus instruction_table
    ; write_bus = combine_write_bus instruction_table
    }
  ;;

  let hierarchical (scope : Scope.t) (input : Signal.t I.t) =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ~scope ~name:"execute" create input
  ;;
end
