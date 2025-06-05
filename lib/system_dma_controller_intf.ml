open! Core
open Hardcaml
open Hardcaml_memory_controller
open Hardcaml_io_controller

(* TODO: This file does too much wiring. Split it out and clean it up. *)

module M
    (General_config : System_intf.Config)
    (Memory : Memory_bus_intf.S)
    (Memory_to_packet8 : Memory_to_packet8_intf.M(Memory)(Axi8).S) =
struct
  module type S = sig
    type 'a t =
      { write_request : Signal.t Memory.Write_bus.Source.t
      ; read_request : Signal.t Memory.Read_bus.Source.t
      ; read_response : Signal.t Memory.Read_response.With_valid.t
      ; write_response : Signal.t Memory.Write_response.With_valid.t
      ; write_bus : Signal.t Memory.Write_bus.Dest.t
      ; read_bus : Signal.t Memory.Read_bus.Dest.t
      ; uart_rx_valid : 'a
      ; tx_input : Signal.t Memory_to_packet8.Input.With_valid.t
      ; tx_busy : 'a
      ; uart_tx : Signal.t
      ; parity_error : 'a
      ; serial_to_packet_valid : 'a
      ; clear_message : 'a
      }
    [@@deriving fields]

    val maybe_dma_controller
      :  uart_rx:Signal.t option
      -> clock:Signal.t
      -> clear:Signal.t
      -> Scope.t
      -> Signal.t t option
  end
end
