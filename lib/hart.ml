module Make (Hart_config : Hart_config_intf.S) (Memory : Memory_bus.S) = struct
  module Registers = Registers.Make (Hart_config)

  module State = struct
    type t =
      | Fetching
      | Decode_and_execute
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
    let reg_spec = Reg_spec.create ~clock:i.clock ~clear:i.clear in
    let registers = Registers.Of_always.reg reg_spec in
    assert false
  ;;
end
