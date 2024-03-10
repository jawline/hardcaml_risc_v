open! Core
open Hardcaml
open Signal
open Always

module Make (Hart_config : Hart_config_intf.S) (Memory : Memory_bus_intf.S) = struct
  module Registers = Registers.Make (Hart_config)
  module Fetch = Fetch.Make (Hart_config) (Memory)

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
      ; hart_to_memory_controller : 'a Memory.Tx_bus.Rx.t
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t =
      { memory_controller_to_hart : 'a Memory.Rx_bus.Rx.t
      ; hart_to_memory_controller : 'a Memory.Tx_bus.Tx.t
      }
    [@@deriving sexp_of, hardcaml]
  end

  let create scope (i : _ I.t) =
    let reg_spec = Reg_spec.create ~clock:i.clock ~clear:i.clear () in
    let registers = Registers.Of_always.reg reg_spec in
    let current_state = Always.State_machine.create (module State) ~enable:vdd reg_spec in
    let _memory_controller_to_hart = Memory.Rx_bus.Rx.Of_always.wire zero in
    let _hart_to_memory_controller = Memory.Tx_bus.Tx.Of_always.wire zero in
    let _fetch =
      Fetch.hierarchical
        ~instance:"fetcher"
        scope
        { Fetch.I.memory_controller_to_hart = i.memory_controller_to_hart
        ; hart_to_memory_controller = i.hart_to_memory_controller
        ; should_fetch = current_state.is State.Fetching
        ; address = registers.pc.value
        }
    in
    compile [ current_state.switch [ State.Fetching, []; Decode_and_execute, [] ] ];
    assert false
  ;;
end
