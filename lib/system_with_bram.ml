open! Core
open Hardcaml
open Hardcaml_memory_controller
open Hardcaml_risc_v_hart
open Signal

module Make
    (Hart_config : Hart_config_intf.S)
    (Memory_config : System_intf.Memory_config)
    (General_config : System_intf.Config) =
struct
  module Axi_config = struct
    let id_width = 8
    let data_width = 32
    let addr_width = address_bits_for (Memory_config.capacity_in_bytes / (data_width / 8))
  end

  module Axi4 = Axi4.Make (Axi_config)

  module Memory =
    Axi4_bram.Make
      (struct
        let capacity_in_bytes = Memory_config.capacity_in_bytes
      end)
      (Axi_config)
      (Axi4)

  module System =
    System.Make (Hart_config) (Memory_config) (General_config) (Axi_config) (Axi4)

  (* Re-exports *)
  include struct
    open System

    let include_uart_wires = include_uart_wires
    let include_video_out = include_video_out

    module Memory_bus = Memory_bus
    module Registers = Registers
    module Video_out_with_memory = Video_out_with_memory
  end

  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; uart_rx : 'a option [@exists include_uart_wires]
      }
    [@@deriving hardcaml ~rtlmangle:"$"]
  end

  module O = struct
    type 'a t =
      { registers : 'a Registers.t list [@length General_config.num_harts]
      ; uart_tx : 'a option [@exists include_uart_wires]
      ; uart_rx_valid : 'a option [@exists include_uart_wires]
      ; video_out : 'a Video_out_with_memory.O.t option [@exists include_video_out]
      }
    [@@deriving hardcaml ~rtlmangle:"$"]
  end

  let create ~build_mode ~read_latency scope { I.clock; clear; uart_rx } =
    let axi = Axi4.O.Of_signal.wires () in
    let mem =
      Memory.hierarchical ~build_mode ~read_latency scope { Memory.I.clock; clear; axi }
    in
    let core =
      System.hierarchical
        ~build_mode
        scope
        { System.I.clock; clear; uart_rx; memory = mem.axi }
    in
    Axi4.O.Of_signal.assign axi core.memory;
    { O.registers = core.registers
    ; uart_tx = core.uart_tx
    ; uart_rx_valid = core.uart_rx_valid
    ; video_out = core.video_out
    }
  ;;

  let hierarchical ~build_mode ~read_latency (scope : Scope.t) (input : Signal.t I.t) =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical
      ~scope
      ~name:"system_with_bram"
      (create ~build_mode ~read_latency)
      input
  ;;
end
