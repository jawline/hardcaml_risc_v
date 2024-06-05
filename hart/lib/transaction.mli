open! Core
open! Hardcaml
open Hardcaml_memory_controller

module Make (Hart_config : Hart_config_intf.S) (Memory : Memory_bus_intf.S) :
  Transaction_intf.S
