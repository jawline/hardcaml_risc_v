(** This module constructs an entire system, including harts, a memory
    controller, and optionally an IO controller and a video signal generator
    that generates a framebuffer by expanding pixels in memory. The system is
    defined by three parts, a general config which decides what features to
    enable, a memory config that describes the shape of the memory controller,
    and a hart config which configures each hart. All harts must be homogeneous. *)
open! Core

open Hardcaml
open Hardcaml_memory_controller
open Hardcaml_risc_v_hart

module Make
    (Hart_config : Hart_config_intf.S)
    (Memory_config : System_intf.Memory_config)
    (General_config : System_intf.Config)
    (Axi4 : Axi4.S) : sig
  module Registers : Registers_intf.S
  module Memory_bus : Memory_bus_intf.S
  module Video_out_with_memory : Video_out_intf.M(Memory_bus).S

  module I : sig
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; uart_rx : 'a option
      ; memory : 'a Axi4.I.t
      }
    [@@deriving hardcaml]
  end

  module O : sig
    type 'a t =
      { registers : 'a Registers.t list [@length General_config.num_harts]
      ; uart_tx : 'a option
      ; uart_rx_valid : 'a option
      ; video_out : 'a Video_out_with_memory.O.t option
      ; memory : 'a Axi4.O.t
      }
    [@@deriving hardcaml]
  end

  val include_uart_wires : bool
  val include_video_out : bool
  val hierarchical : build_mode:Build_mode.t -> Scope.t -> Signal.t I.t -> Signal.t O.t
end
