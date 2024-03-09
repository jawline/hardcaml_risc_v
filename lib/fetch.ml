open Core
open Hardcaml
open Signal
open Always

module Make (Hart_config : Hart_config_intf.S) (Memory : Memory_bus.S) = struct
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
    let should_fetch = i.memory_controller_to_hart.ready &: i.should_fetch in
    { memory_controller_to_hart = Memory.Rx_bus.Rx.Of_signal.of_int 1
    ; hart_to_memory_controller =
        { Memory.Tx_bus.Tx.valid = should_fetch
        ; address = i.address
        ; write = zero 1
        ; data = zero 1
        }
    ; has_fetched = i.memory_controller_to_hart.valid
    ; instruction = i.memory_controler_to_hart.data
    }
  ;;
end
