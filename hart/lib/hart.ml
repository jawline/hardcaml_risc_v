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
  module Fetch = Fetch.Make (Hart_config) (Memory)

  module Decode_and_execute =
    Decode_and_execute.Make (Hart_config) (Memory) (Registers) (Decoded_instruction)
      (Transaction)

  module State = struct
    type t =
      | Fetching
      | Decode_and_execute
    [@@deriving sexp_of, compare, enumerate]
  end

  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; memory_controller_to_hart : 'a Memory.Rx_bus.Tx.t
           [@rtlprefix "memory_controller_to_hart"]
      ; hart_to_memory_controller : 'a Memory.Tx_bus.Rx.t
           [@rtlprefix "hart_to_memory_controller"]
      ; (* When is_ecall is high the opcode will be considered finished when
           ecall_transaction is finished. If a user wants custom behaviour on ecall
           they should hold ecall finished low, do the work, then raise finished. *)
        ecall_transaction : 'a Transaction.t
      }
    [@@deriving sexp_of, hardcaml ~rtlmangle:true]
  end

  module O = struct
    type 'a t =
      { memory_controller_to_hart : 'a Memory.Rx_bus.Rx.t
           [@rtlprefix "memory_controller_to_hart"]
      ; hart_to_memory_controller : 'a Memory.Tx_bus.Tx.t
           [@rtlprefix "hart_to_memory_controller"]
      ; registers : 'a Registers.t
      ; error : 'a
      ; (* Set high when the hart is in an ecall and is delagating behaviour to
           the user design. *) is_ecall : 'a
      }
    [@@deriving sexp_of, hardcaml ~rtlmangle:true]
  end

  let create scope (i : _ I.t) =
    let ( -- ) = Scope.naming scope in
    let reg_spec = Reg_spec.create ~clock:i.clock ~clear:i.clear () in
    let reg_spec_no_clear = Reg_spec.create ~clock:i.clock () in
    (* Register 0 is hardwired to zero so we don't actually store it *)
    let current_state = State_machine.create (module State) ~enable:vdd reg_spec in
    ignore (current_state.current -- "current_state" : Signal.t);
    let registers = Registers.For_writeback.Of_always.reg reg_spec in
    let memory_controller_to_hart = Memory.Rx_bus.Rx.Of_always.wire zero in
    let hart_to_memory_controller = Memory.Tx_bus.Tx.Of_always.wire zero in
    let error = Variable.wire ~default:(zero 1) in
    let fetched_instruction =
      Variable.reg
        ~width:(Register_width.bits Hart_config.register_width)
        reg_spec_no_clear
    in
    let fetch =
      Fetch.hierarchical
        ~instance:"fetcher"
        scope
        { Fetch.I.memory_controller_to_hart = i.memory_controller_to_hart
        ; hart_to_memory_controller = i.hart_to_memory_controller
        ; should_fetch = current_state.is State.Fetching
        ; address = registers.pc.value
        }
    in
    let decode_and_execute =
      Decode_and_execute.hierarchical
        ~instance:"decode_and_execute"
        scope
        { Decode_and_execute.I.clock = i.clock
        ; clear = i.clear
        ; memory_controller_to_hart = i.memory_controller_to_hart
        ; hart_to_memory_controller = i.hart_to_memory_controller
        ; enable = current_state.is State.Decode_and_execute
        ; instruction = fetched_instruction.value
        ; registers =
            Registers.For_writeback.Of_always.value registers
            |> Registers.For_writeback.to_registers
        ; ecall_transaction = i.ecall_transaction
        }
    in
    compile
      [ current_state.switch
          [ ( State.Fetching
            , [ Memory.Rx_bus.Rx.Of_always.assign
                  memory_controller_to_hart
                  fetch.memory_controller_to_hart
              ; Memory.Tx_bus.Tx.Of_always.assign
                  hart_to_memory_controller
                  fetch.hart_to_memory_controller
              ; fetched_instruction <-- fetch.instruction
              ; error <-- fetch.error
              ; when_ fetch.has_fetched [ current_state.set_next Decode_and_execute ]
              ] )
          ; ( Decode_and_execute
            , [ Memory.Rx_bus.Rx.Of_always.assign
                  memory_controller_to_hart
                  decode_and_execute.memory_controller_to_hart
              ; Memory.Tx_bus.Tx.Of_always.assign
                  hart_to_memory_controller
                  decode_and_execute.hart_to_memory_controller
              ; when_
                  decode_and_execute.finished
                  [ Registers.For_writeback.Of_always.assign
                      registers
                      decode_and_execute.new_registers
                  ; current_state.set_next Fetching
                  ]
              ] )
          ]
      ];
    { O.memory_controller_to_hart =
        Memory.Rx_bus.Rx.Of_always.value memory_controller_to_hart
    ; hart_to_memory_controller =
        Memory.Tx_bus.Tx.Of_always.value hart_to_memory_controller
    ; registers =
        Registers.For_writeback.Of_always.value registers
        |> Registers.For_writeback.to_registers
    ; error = error.value
    ; is_ecall = decode_and_execute.is_ecall
    }
  ;;

  let hierarchical ~instance (scope : Scope.t) (input : Signal.t I.t) =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ~scope ~name:"hart" ~instance create input
  ;;
end
