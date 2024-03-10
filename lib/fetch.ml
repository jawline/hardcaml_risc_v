open Core
open Hardcaml
open Signal
open Always

module Make (Hart_config : Hart_config_intf.S) (Memory : Memory_bus_intf.S) = struct
  module I = struct
    type 'a t =
      { memory_controller_to_hart : 'a Memory.Rx_bus.Tx.t
      ; hart_to_memory_controller : 'a Memory.Tx_bus.Rx.t
      ; should_fetch : 'a
      ; address : 'a [@bits Address_width.bits Hart_config.address_width]
      }
  end

  module O = struct
    type 'a t =
      { memory_controller_to_hart : 'a Memory.Rx_bus.Rx.t
      ; hart_to_memory_controller : 'a Memory.Tx_bus.Tx.t
      ; has_fetched : 'a
      ; instruction : 'a [@bits 32]
      }
  end

  let create scope (i : _ I.t) =
    let should_fetch = i.hart_to_memory_controller.ready &: i.should_fetch in
    { O.memory_controller_to_hart = Memory.Rx_bus.Rx.Of_signal.of_int 1
    ; hart_to_memory_controller =
        { Memory.Tx_bus.Tx.valid = should_fetch
        ; data =
            { Memory.Tx_data.address = i.address; write = zero 1; write_data = zero 32 }
        }
    ; has_fetched =
        i.memory_controller_to_hart.valid &: ~:(i.memory_controler_to_hart.error)
    ; instruction = i.memory_controler_to_hart.read_data
    }
  ;;

  let hierarchical ~instance (scope : Scope.t) (input : Signal.t I.t) =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ~scope ~name:"Fetch" ~instance create input
  ;;
end
