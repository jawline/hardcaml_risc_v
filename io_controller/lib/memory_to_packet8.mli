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
open Hardcaml
open Hardcaml_axi
open Hardcaml_memory_controller

module Make
    (Config : Memory_to_packet8_intf.Config)
    (Memory : Memory_bus_intf.S)
    (Axi : Stream.S) : sig
  module Input : sig
    type 'a t =
      { length : 'a [@bits 16]
      ; address : 'a [@bits Memory.data_bus_width]
      }
    [@@deriving hardcaml]

    module With_valid : With_valid.Wrap.S with type 'a value := 'a t
  end

  module I : sig
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; enable : 'a Input.With_valid.t
      ; output_packet : 'a Axi.Dest.t
      ; memory : 'a Memory.Read_bus.Dest.t
      ; memory_response : 'a Memory.Read_response.With_valid.t
      }
    [@@deriving hardcaml]
  end

  module O : sig
    type 'a t =
      { busy : 'a
      ; done_ : 'a
      ; output_packet : 'a Axi.Source.t [@rtlprefix "output$"]
      ; memory : 'a Memory.Read_bus.Source.t [@rtlprefix "memory$"]
      }
    [@@deriving hardcaml]
  end

  val hierarchical : Scope.t -> Signal.t I.t -> Signal.t O.t
end
