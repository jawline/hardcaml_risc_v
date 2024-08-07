open! Core
open Hardcaml
open Hardcaml_memory_controller
open Signal

module Make (Hart_config : Hart_config_intf.S) (Memory : Memory_bus_intf.S) = struct
  module I = struct
    type 'a t =
      { memory_controller_to_hart : 'a Memory.Rx_bus.Tx.t
           [@rtlprefix "memory_controller_to_hart"]
      ; hart_to_memory_controller : 'a Memory.Tx_bus.Rx.t
           [@rtlprefix "hart_to_memory_controller"]
      ; should_fetch : 'a
      ; address : 'a [@bits Register_width.bits Hart_config.register_width]
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t =
      { memory_controller_to_hart : 'a Memory.Rx_bus.Rx.t
           [@rtlprefix "memory_controller_to_hart"]
      ; hart_to_memory_controller : 'a Memory.Tx_bus.Tx.t
           [@rtlprefix "hart_to_memory_controller"]
      ; has_fetched : 'a
      ; instruction : 'a [@bits 32]
      ; error : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  let create _scope (i : _ I.t) =
    let should_fetch = i.should_fetch in
    { O.memory_controller_to_hart = Memory.Rx_bus.Rx.Of_signal.of_int 1
    ; hart_to_memory_controller =
        { Memory.Tx_bus.Tx.valid = should_fetch
        ; data = { Memory.Tx_data.address = i.address; write = gnd; write_data = zero 32 }
        }
    ; has_fetched =
        i.memory_controller_to_hart.valid &: ~:(i.memory_controller_to_hart.data.error)
    ; instruction = i.memory_controller_to_hart.data.read_data
    ; error = i.memory_controller_to_hart.valid &: i.memory_controller_to_hart.data.error
    }
  ;;

  let hierarchical ~instance (scope : Scope.t) (input : Signal.t I.t) =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ~scope ~name:"fetch" ~instance create input
  ;;
end
