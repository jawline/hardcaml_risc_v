(** This module extends system with a BRAM based memory for a simple core. *)
open! Core

open Hardcaml
open Hardcaml_memory_controller
open Hardcaml_risc_v_hart

module Make
    (Hart_config : Hart_config_intf.S)
    (Memory_config : System_intf.Memory_config)
    (General_config : System_intf.Config) : sig
  module Registers : Registers_intf.S
  module Memory_bus : Memory_bus_intf.S
  module Video_out_with_memory : Video_out_intf.M(Memory_bus).S

  module I : sig
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; uart_rx : 'a option
      }
    [@@deriving hardcaml]
  end

  module O : sig
    type 'a t =
      { registers : 'a Registers.t list [@length General_config.num_harts]
      ; uart_tx : 'a option
      ; uart_rx_valid : 'a option
      ; video_out : 'a Video_out_with_memory.O.t option
      }
    [@@deriving hardcaml]
  end

  val hierarchical
    :  build_mode:Build_mode.t
    -> read_latency:int
    -> Scope.t
    -> Signal.t I.t
    -> Signal.t O.t
end
