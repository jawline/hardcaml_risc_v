module Make (Memory : Memory_bus.S) = struct
  module I = struct
    type 'a t =
      { memory_controller_to_hart : 'a Memory.Rx_bus.Tx.t
      ; hart_to_memory_controller : 'a Memory.Tx_bus.Rx.t
      }
  end

  module O = struct
    type 'a t =
      { memory_controller_to_hart : 'a Memory.Rx_bus.Rx.t
      ; hart_to_memory_controller : 'a Memory.Tx_bus.Tx.t
      }
  end

  let create scope (i : _ I.t) = assert false
end
