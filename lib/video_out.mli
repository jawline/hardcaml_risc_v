open! Core
open Hardcaml_memory_controller
module Make (Memory : Memory_bus_intf.S) : Video_out_intf.M(Memory).S
