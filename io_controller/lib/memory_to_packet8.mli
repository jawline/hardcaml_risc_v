(* Write a packet from memory. The framing format is a 2 byte length tag
   followed by data.

   The module takes an enable signal with a length and address and writes
   out the length (Little-endian) and then the memory byte by byte to
   an output stream. When connected to a Uart_tx this should allow us
   to communicate with the host.

   Currently this module does not prefetch memory while writing, which would
   be a straightforward improvement.
*)
open! Core
open Hardcaml_axi
open Hardcaml_memory_controller

module Make
    (Config : Memory_to_packet8_intf.Config)
    (Memory : Memory_bus_intf.S)
    (Axi : Stream.S) : Memory_to_packet8_intf.M(Memory)(Axi).S
