open! Core
open Hardcaml
open Signal
open Always

module Make (Hart_config : Hart_config_intf.S) (Memory : Memory_bus_intf.S) = struct
  module Registers = Registers.Make (Hart_config)
  module Fetch = Fetch.Make (Hart_config) (Memory)
  module Decode_and_execute = Decode_and_execute.Make (Hart_config) (Memory) (Registers)

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
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t =
      { memory_controller_to_hart : 'a Memory.Rx_bus.Rx.t
           [@rtlprefix "memory_controller_to_hart"]
      ; hart_to_memory_controller : 'a Memory.Tx_bus.Tx.t
           [@rtlprefix "hart_to_memory_controller"]
      ; registers :
          (* TODO: I've stuck this in mostly to avoid the whole
             design getting const-prop deleted for testing. Remove it
             after this is IO in the top level design. *)
          'a Registers.t
      ; error : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  let create scope (i : _ I.t) =
    let reg_spec = Reg_spec.create ~clock:i.clock ~clear:i.clear () in
    let registers = Registers.Of_always.reg reg_spec in
    let current_state = State_machine.create (module State) ~enable:vdd reg_spec in
    let memory_controller_to_hart = Memory.Rx_bus.Rx.Of_always.wire zero in
    let hart_to_memory_controller = Memory.Tx_bus.Tx.Of_always.wire zero in
    let error = Variable.wire ~default:(zero 1) in
    let fetched_instruction =
      Variable.reg ~width:(Address_width.bits Hart_config.address_width) reg_spec
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
        ; registers = Registers.Of_always.value registers
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
            , [ current_state.set_next Fetching
              ; registers.pc <-- registers.pc.value +:. 4
              ; Memory.Rx_bus.Rx.Of_always.assign
                  memory_controller_to_hart
                  decode_and_execute.memory_controller_to_hart
              ; Memory.Tx_bus.Tx.Of_always.assign
                  hart_to_memory_controller
                  decode_and_execute.hart_to_memory_controller
              ; when_
                  decode_and_execute.finished
                  [ Registers.Of_always.assign registers decode_and_execute.new_registers
                  ]
              ] )
          ]
      ];
    { O.memory_controller_to_hart =
        Memory.Rx_bus.Rx.Of_always.value memory_controller_to_hart
    ; hart_to_memory_controller =
        Memory.Tx_bus.Tx.Of_always.value hart_to_memory_controller
    ; registers = Registers.Of_always.value registers
    ; error = error.value
    }
  ;;

  let hierarchical ~instance (scope : Scope.t) (input : Signal.t I.t) =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ~scope ~name:"Hart" ~instance create input
  ;;
end
