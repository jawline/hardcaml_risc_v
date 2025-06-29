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
    module I : sig
      type 'a t =
        { clock : 'a
        ; clear : 'a
        ; uart_rx : 'a
        ; tx_input : 'a Memory_to_packet8.Input.With_valid.t
        ; read_request : 'a Memory.Read_bus.Dest.t
        ; write_request : 'a Memory.Write_bus.Dest.t
        ; read_response : 'a Memory.Read_response.With_valid.t
        ; write_response : 'a Memory.Write_response.With_valid.t
        }
      [@@deriving hardcaml]
    end

    module O : sig
      type 'a t =
        { uart_rx_valid : 'a
        ; uart_tx : 'a
        ; dma_tx_ready : 'a
        ; read_request : 'a Memory.Read_bus.Source.t
        ; write_request : 'a Memory.Write_bus.Source.t
        ; clear_message : 'a
        }
      [@@deriving hardcaml, fields ~getters]
    end

    val create
      :  uart_config:Hardcaml_uart.Config.t
      -> Scope.t
      -> Signal.t I.t
      -> Signal.t O.t

    val hierarchical
      :  ?instance:string
      -> uart_config:Hardcaml_uart.Config.t
      -> Scope.t
      -> Signal.t I.t
      -> Signal.t O.t
  end
end
