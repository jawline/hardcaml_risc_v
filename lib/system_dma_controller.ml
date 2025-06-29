open! Core
open Hardcaml
open Hardcaml_memory_controller
open Hardcaml_io_framework
open Hardcaml_io_controller
open Hardcaml_circuits
open Hardcaml_uart
open Signal

(* TODO: This file does too much wiring. Split it out and clean it up. *)

module Make
    (General_config : System_intf.Config)
    (Memory : Memory_bus_intf.S)
    (Memory_to_packet8 : Memory_to_packet8_intf.M(Memory)(Axi8).S) =
struct
  module Write_dpath = Datapath_register.Make (Memory.Write)

  module I = struct
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
    [@@deriving hardcaml ~rtlmangle:"$"]
  end

  module O = struct
    type 'a t =
      { uart_rx_valid : 'a
      ; uart_tx : 'a
      ; dma_tx_ready : 'a
      ; read_request : 'a Memory.Read_bus.Source.t
      ; write_request : 'a Memory.Write_bus.Source.t
      ; clear_message : 'a
      }
    [@@deriving hardcaml ~rtlmangle:"$", fields ~getters]
  end

  type 'a t =
    { write_request : Signal.t Memory.Write_bus.Source.t
    ; read_request : Signal.t Memory.Read_bus.Source.t
    ; read_response : Signal.t Memory.Read_response.With_valid.t
    ; write_response : Signal.t Memory.Write_response.With_valid.t
    ; write_bus : Signal.t Memory.Write_bus.Dest.t
    ; read_bus : Signal.t Memory.Read_bus.Dest.t
    ; uart_rx_valid : 'a
    ; tx_input : Signal.t Memory_to_packet8.Input.With_valid.t
    ; dma_tx_ready : 'a
    ; uart_tx : Signal.t
    ; parity_error : 'a
    ; serial_to_packet_valid : 'a
    ; clear_message : 'a
    }
  [@@deriving fields]

  let create
        ~uart_config
        scope
        { I.clock
        ; clear
        ; uart_rx
        ; tx_input
        ; read_request
        ; write_request
        ; read_response
        ; write_response
        }
    =
    let module Config = struct
      let config = uart_config
    end
    in
    let module Dma = Packet_to_memory.Make (Memory) (Axi8) in
    let module Uart_tx = Uart_tx.Make (Config) in
    let module Uart_rx = Uart_rx.Make (Config) in
    let module Serial_buffer =
      Serial_buffer.Make (struct
        let serial_input_width = 8
      end)
    in
    let module Serial_to_packet =
      Serial_to_packet.Make
        (struct
          let header = 'Q'
        end)
        (Axi8)
    in
    let module Router =
      Router.Make
        (struct
          let num_tags = 2
        end)
        (Axi8)
    in
    let module Pulse = Pulse.Make (Axi8) in
    let { Uart_rx.O.data_out_valid = uart_rx_valid
        ; data_out = uart_rx_data
        ; parity_error = _
        }
      =
      Uart_rx.hierarchical scope { Uart_rx.I.clock; clear; uart_rx }
    in
    let serial_to_packet_ready = wire 1 in
    let { Serial_buffer.O.out_valid = serial_buffer_valid; out_data = serial_buffer_data }
      =
      Serial_buffer.hierarchical
        ~capacity:2048
        scope
        { Serial_buffer.I.clock
        ; clear
        ; in_valid = uart_rx_valid
        ; in_data = uart_rx_data
        ; out_ready = serial_to_packet_ready
        }
    in
    let dpath_ready = wire 1 in
    let { Serial_to_packet.O.dn; up_ready = serial_to_packet_ready' } =
      Serial_to_packet.hierarchical
        scope
        { Serial_to_packet.I.clock
        ; clear
        ; in_valid = serial_buffer_valid
        ; in_data = serial_buffer_data
        ; dn = { tready = dpath_ready }
        }
    in
    let router_ready = wire 1 in
    let { Axi8.Datapath_register.IO.source = dn; dest = dpath_ready' } =
      Axi8.Datapath_register.hierarchical
        scope
        { clock
        ; clear
        ; i = { Axi8.Datapath_register.IO.source = dn; dest = { tready = router_ready } }
        }
    in
    dpath_ready <-- dpath_ready'.tready;
    serial_to_packet_ready <-- serial_to_packet_ready';
    let dma_dpath_ready = wire 1 in
    let pulse_ready = wire 1 in
    let router =
      Router.hierarchical
        scope
        { Router.I.clock
        ; clear
        ; up = dn
        ; dns = [ { tready = dma_dpath_ready }; { tready = pulse_ready } ]
        }
    in
    router_ready <-- router.up.tready;
    let dma_ready = wire 1 in
    let { Axi8.Datapath_register.IO.source = dma; dest = dpath_ready' } =
      Axi8.Datapath_register.hierarchical
        scope
        { clock
        ; clear
        ; i =
            { Axi8.Datapath_register.IO.source = List.nth_exn router.dns 0
            ; dest = { tready = dma_ready }
            }
        }
    in
    dma_dpath_ready <-- dpath_ready'.tready;
    let dma_write_dpath_ready = wire 1 in
    let dma =
      Dma.hierarchical
        ~instance:"dma"
        scope
        { Dma.I.clock; clear; in_ = dma; out = write_request; out_ack = write_response }
    in
    dma_ready <-- dma.in_.tready;
    let dma_write_dpath_reg =
      Write_dpath.hierarchical
        scope
        { Write_dpath.I.clock
        ; clear
        ; i = { valid = dma.out.valid; data = dma.out.data; ready = write_request.ready }
        }
    in
    dma_write_dpath_ready <-- dma_write_dpath_reg.ready;
    let pulse =
      Pulse.hierarchical scope { Pulse.I.clock; clear; up = List.nth_exn router.dns 1 }
    in
    pulse_ready <-- pulse.up.tready;
    let uart_tx_ready = wire 1 in
    let dma_out =
      Memory_to_packet8.hierarchical
        scope
        { Memory_to_packet8.I.clock
        ; clear
        ; enable = tx_input
        ; memory = read_request
        ; memory_response = read_response
        ; output_packet = { tready = uart_tx_ready }
        }
    in
    let dma_out_uart_tx =
      Uart_tx.hierarchical
        scope
        { Uart_tx.I.clock
        ; clear
        ; data_in_valid = dma_out.output_packet.tvalid
        ; data_in = dma_out.output_packet.tdata
        }
    in
    uart_tx_ready <-- dma_out_uart_tx.data_in_ready;
    { O.write_request =
        { valid = dma_write_dpath_reg.valid; data = dma_write_dpath_reg.data }
    ; read_request = dma_out.memory
    ; uart_tx = dma_out_uart_tx.uart_tx
    ; uart_rx_valid
    ; clear_message = pulse.signal
    ; dma_tx_ready = dma_out.ready
    }
  ;;

  let hierarchical ?instance ~uart_config (scope : Scope.t) (input : Signal.t I.t) =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical
      ?instance
      ~scope
      ~name:"system_dma_controller"
      (create ~uart_config)
      input
  ;;
end
