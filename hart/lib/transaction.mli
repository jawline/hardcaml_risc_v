open! Core
open! Hardcaml
open Hardcaml_memory_controller

module Make (Hart_config : Hart_config_intf.S) (Memory : Memory_bus_intf.S) : sig
  type 'a t =
    { finished : 'a
    ; set_rd : 'a
    ; new_rd : 'a [@bits register_width]
    ; new_pc : 'a [@bits register_width]
    ; error : 'a
    }
  [@@deriving sexp_of, hardcaml]
end
