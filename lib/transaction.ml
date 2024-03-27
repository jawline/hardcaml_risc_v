open! Core
open! Hardcaml

module Make (Hart_config : Hart_config_intf.S) (Memory : Memory_bus_intf.S) = struct
  let register_width = Register_width.bits Hart_config.register_width

  type 'a t =
    { (* We could pull old_rd into decoded instruction to avoid the gate on committing *)
      finished : 'a
    ; set_rd : 'a
    ; new_rd : 'a [@bits register_width]
    ; new_pc : 'a [@bits register_width]
    ; error : 'a
    ; memory_controller_to_hart : 'a Memory.Rx_bus.Rx.t
    ; hart_to_memory_controller : 'a Memory.Tx_bus.Tx.t
    }
  [@@deriving sexp_of, hardcaml]
end
