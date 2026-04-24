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
  module Op_with_mul = Op_with_mul.Make (Hart_config)
  module Branch = Branch.Make (Hart_config)
  module Load = Load.Make (Hart_config) (Memory)
  module Store = Store.Make (Hart_config) (Memory)

  let register_width = Register_width.bits Hart_config.register_width
  let instruction_size_in_bytes = 4
  let next_instruction = incr ~by:instruction_size_in_bytes

  module I = struct
    type 'a t =
      { clock : 'a Clocking.t
      ; valid : 'a [@rtlname "input_valid"]
      ; instruction : 'a Decoded_instruction.t
      ; ecall_transaction : 'a Transaction.With_valid.t
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
      ; instruction : 'a Decoded_instruction.t
      ; transaction : 'a Transaction.t
      ; is_ecall : 'a
      ; write_bus : 'a Memory.Write_bus.Source.t
      ; read_bus : 'a Memory.Read_bus.Source.t
      ; error : 'a
      }
    [@@deriving hardcaml ~rtlmangle:"$output_"]
  end

  module Without_registers = struct
    module Opcode_output = struct
      type 'a t =
        { valid : 'a
        ; transaction : 'a Transaction.t
        ; write_bus : 'a Memory.Write_bus.Source.t option
        ; read_bus : 'a Memory.Read_bus.Source.t option
        }
      [@@deriving fields ~getters]
    end

    let op_instructions
          ~valid
          ~clock
          scope
          ({ pc
           ; opcode
           ; argument_1
           ; argument_2
           ; alu_operation
           ; muldiv_operation
           ; is_muldiv
           ; _
           } :
            _ Decoded_instruction.t)
      =
      let alu_input = valid &: Decoded_opcode.valid opcode ALU in
      let valid, new_rd =
        if Hart_config.Extensions.zmul
        then (
          let { Op_with_mul.O.valid; rd = new_rd } =
            Op_with_mul.hierarchical
              scope
              { Op_with_mul.I.clock
              ; valid = alu_input
              ; alu_op = alu_operation
              ; muldiv_op = Option.value_exn muldiv_operation
              ; is_muldiv = Option.value_exn is_muldiv
              ; lhs = argument_1
              ; rhs = argument_2
              }
          in
          valid, new_rd)
        else (
          let { Op.O.valid; rd = new_rd } =
            Op.hierarchical
              scope
              { Op.I.valid = alu_input
              ; op = alu_operation
              ; lhs = argument_1
              ; rhs = argument_2
              }
          in
          valid, new_rd)
      in
      { Opcode_output.valid
      ; read_bus = None
      ; write_bus = None
      ; transaction =
          { Transaction.set_rd = vdd; new_rd; error = gnd; new_pc = next_instruction pc }
      }
    ;;

    (** Assign PC = argument 1 + argument 2, set rd = pc + 4. Implements JAL and JALR. *)
    let assign_pc_sum_of_arguments
          ~valid
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
          { Transaction.set_rd = vdd
          ; new_rd = next_instruction decoded_instruction.pc
          ; new_pc
          ; error = gnd
          }
      }
    ;;

    (** The branch table either compares rs1 and rs2 based on funct3 and then
      either adds a b immediate to the PC or skips to the next instruction
      based on the result. *)
    let branch_instruction ~valid (decoded_instruction : _ Decoded_instruction.t) scope =
      let { Branch.O.new_pc } =
        Branch.hierarchical
          scope
          { Branch.I.op = decoded_instruction.branch_onehot
          ; lhs = decoded_instruction.argument_1
          ; rhs = decoded_instruction.argument_2
          ; branch_offset = decoded_instruction.argument_3
          ; pc = decoded_instruction.pc
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

    (** The load table loads a value from [rs1] and places it in rd *)
    let load_instruction
          ~clock
          ~valid
          ~read_bus
          ~read_response
          (decoded_instruction : _ Decoded_instruction.t)
          scope
      =
      let { Load.O.finished; new_rd; error; read_bus } =
        Load.hierarchical
          scope
          { Load.I.clock
          ; enable =
              (* We need to guard the Load instruction since it's internal
               state machine might try to load data and get stuck otherwise. *)
              valid &: Decoded_opcode.valid decoded_instruction.opcode Load
          ; op = decoded_instruction.load_onehot
          ; address = decoded_instruction.argument_1 +: decoded_instruction.argument_3
          ; read_bus
          ; read_response
          }
      in
      { Opcode_output.valid = finished
      ; read_bus = Some read_bus
      ; write_bus = None
      ; transaction =
          { Transaction.set_rd = vdd
          ; new_rd
          ; error
          ; new_pc = next_instruction decoded_instruction.pc
          }
      }
    ;;

    (** The store table loads a value from [rs1] and writes it to address rd *)
    let store_instruction
          ~clock
          ~valid
          ~write_bus
          ~write_response
          (decoded_instruction : _ Decoded_instruction.t)
          scope
      =
      let { Store.O.finished; error; write_bus } =
        Store.hierarchical
          scope
          { Store.I.clock
          ; enable =
              (* We need to guard the Store instruction since it's internal
               state machine might try to load data and get stuck otherwise. *)
              valid &: Decoded_opcode.valid decoded_instruction.opcode Store
          ; op = decoded_instruction.store_onehot
          ; destination = decoded_instruction.argument_1 +: decoded_instruction.argument_3
          ; value = decoded_instruction.argument_2
          ; write_bus
          ; write_response
          }
      in
      { Opcode_output.valid = finished
      ; transaction =
          { Transaction.set_rd = gnd
          ; new_rd = zero register_width
          ; error
          ; new_pc = next_instruction decoded_instruction.pc
          }
      ; read_bus = None
      ; write_bus = Some write_bus
      }
    ;;

    (** The system instruction allows access to hardware registers and ecall /
      ebreak. For ecall, the behaviour is delegated back to the user design
      via the is_ecall and ecall_transaction signals. *)
    let system_instruction
          ~clock
          ~valid
          ~instret
          ~(ecall_transaction : _ Transaction.With_valid.t)
          (decoded_instruction : _ Decoded_instruction.t)
          scope
      =
      let transaction_of t =
        { Transaction.set_rd = vdd
        ; new_rd = t
        ; error = gnd
        ; new_pc = next_instruction decoded_instruction.pc
        }
      in
      let%hw is_ecall =
        Decoded_opcode.valid decoded_instruction.opcode System
        &: decoded_instruction.is_ecall
      in
      let csr =
        Csr.hierarchical
          ~initialize_registers_to:(Bits.zero 64)
          scope
          { Csr.I.clock; valid; instruction = decoded_instruction; instret }
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
          ~valid
          ~instret
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
          (op_instructions ~clock ~valid scope decoded_instruction)
      ; Table_entry.create
          ~opcode:Decoded_opcode.Assign_pc_sum_of_arguments
          (assign_pc_sum_of_arguments ~valid decoded_instruction scope)
      ; Table_entry.create
          ~opcode:Decoded_opcode.Branch
          (branch_instruction ~valid decoded_instruction scope)
      ; Table_entry.create
          ~opcode:Decoded_opcode.Load
          (load_instruction
             ~clock
             ~valid
             ~read_bus
             ~read_response
             decoded_instruction
             scope)
      ; Table_entry.create
          ~opcode:Decoded_opcode.Store
          (store_instruction
             ~clock
             ~valid
             ~write_bus
             ~write_response
             decoded_instruction
             scope)
      ; Table_entry.create
          ~opcode:Decoded_opcode.System
          (system_instruction
             ~clock
             ~valid
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
          (Memory.Read_bus.Source.Of_signal.zero ())
      in
      List.filter_map ~f:(fun t -> t.output.read_bus) instruction_table
      |> List.map ~f:gate
      |> List.fold ~init:(Memory.Read_bus.Source.Of_signal.zero ()) ~f:combine
    ;;

    let combine_write_bus (instruction_table : _ Table_entry.t list) =
      let combine = Memory.Write_bus.Source.map2 ~f:( |: ) in
      let gate (t : _ Memory.Write_bus.Source.t) =
        Memory.Write_bus.Source.Of_signal.mux2
          t.valid
          t
          (Memory.Write_bus.Source.Of_signal.zero ())
      in
      List.filter_map ~f:(fun t -> t.output.write_bus) instruction_table
      |> List.map ~f:gate
      |> List.fold ~init:(Memory.Write_bus.Source.Of_signal.zero ()) ~f:combine
    ;;

    let create scope (i : _ I.t) =
      let instruction_table =
        instruction_table
          ~clock:i.clock
          ~valid:i.valid
          ~instret:i.instret
          ~decoded_instruction:i.instruction
          ~read_bus:i.read_bus
          ~read_response:i.read_response
          ~write_bus:i.write_bus
          ~write_response:i.write_response
          ~ecall_transaction:i.ecall_transaction
          scope
      in
      let valid =
        List.map ~f:Table_entry.output instruction_table
        |> List.map ~f:Opcode_output.valid
        |> List.reduce_exn ~f:( |: )
      in
      let transaction =
        let opcodes_with_valid =
          List.map ~f:Table_entry.output instruction_table
          |> List.map ~f:(fun output ->
            { With_valid.valid = output.valid; value = output.transaction })
        in
        Transaction.Of_signal.onehot_select opcodes_with_valid
      in
      { O.valid
      ; instruction = i.instruction
      ; transaction
      ; error = transaction.error
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
      H.hierarchical ~scope ~name:"execute_base" create input
    ;;
  end

  let create scope (i : _ I.t) =
    let reg_spec_with_clear = Clocking.to_spec i.clock in
    let reg_spec_no_clear = Clocking.to_spec_no_clear i.clock in
    let maybe_register ~f t = if Hart_config.register_execute_output then f t else t in
    let o = Without_registers.hierarchical scope i in
    { O.valid = maybe_register ~f:(reg reg_spec_with_clear) o.valid
    ; instruction =
        maybe_register
          ~f:(Decoded_instruction.Of_signal.reg ~enable:o.valid reg_spec_no_clear)
          o.instruction
    ; transaction =
        maybe_register
          ~f:(Transaction.Of_signal.reg ~enable:o.valid reg_spec_no_clear)
          o.transaction
    ; error = maybe_register ~f:(reg reg_spec_with_clear) o.error
    ; is_ecall = o.is_ecall
    ; read_bus = o.read_bus
    ; write_bus = o.write_bus
    }
  ;;

  let hierarchical (scope : Scope.t) (input : Signal.t I.t) =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ~scope ~name:"execute" create input
  ;;
end
