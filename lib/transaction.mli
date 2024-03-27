open! Core
open! Hardcaml

module Make (Hart_config : Hart_config_intf.S) (Memory : Memory_bus_intf.S) : sig
  type 'a t =
    { finished : 'a
    ; set_rd : 'a
    ; new_rd : 'a [@bits register_width]
    ; new_pc : 'a [@bits register_width]
    ; error : 'a
    ; memory_controller_to_hart : 'a Memory.Rx_bus.Rx.t
    ; hart_to_memory_controller : 'a Memory.Tx_bus.Tx.t
    }
  [@@deriving sexp_of, hardcaml]
end
