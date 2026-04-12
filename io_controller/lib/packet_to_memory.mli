open! Core
open Hardcaml_axi
open Hardcaml_memory_controller

module Make (Memory : Memory_bus_intf.S) (Axi : Stream.S) : Packet_to_memory_intf.M(Memory)(Axi).S
