open! Core
open Hardcaml
open Hardcaml_memory_controller
open Hardcaml_io_framework
open Hardcaml_io_controller
open Hardcaml_uart
open Signal

module Make (General_config : System_intf.Config) (Memory : Memory_bus_intf.S) = struct
  module Memory_to_packet8 =
    Memory_to_packet8.Make
      (struct
        let header = Some 'D'
      end)
      (Memory)
      (Axi8)

  module Tx_input = Memory_to_packet8.Input

  type 'a t =
    { write_request : Signal.t Memory.Write_bus.Source.t
    ; read_request : Signal.t Memory.Read_bus.Source.t
    ; read_response : Signal.t Memory.Read_response.With_valid.t
    ; write_response : Signal.t Memory.Write_response.With_valid.t
    ; write_bus : Signal.t Memory.Write_bus.Dest.t
    ; read_bus : Signal.t Memory.Read_bus.Dest.t
    ; uart_rx_valid : 'a
    ; tx_input : Signal.t Tx_input.With_valid.t
    ; tx_busy : 'a
    ; uart_tx : Signal.t
    ; parity_error : 'a
    ; serial_to_packet_valid : 'a
    ; clear_message : 'a
    }
  [@@deriving fields]

  let dma_controller ~config ~uart_rx ~clock ~clear scope =
    let module Config = struct
      let config = config
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
    let read_bus = Memory.Read_bus.Dest.Of_signal.wires () in
    let write_bus = Memory.Write_bus.Dest.Of_signal.wires () in
    let read_response = Memory.Read_response.With_valid.Of_signal.wires () in
    let write_response = Memory.Write_response.With_valid.Of_signal.wires () in
    let { Uart_rx.O.data_out_valid = uart_rx_valid
        ; data_out = uart_rx_data
        ; parity_error
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
    let router_ready = wire 1 in
    let { Serial_to_packet.O.dn; up_ready = serial_to_packet_ready' } =
      Serial_to_packet.hierarchical
        scope
        { Serial_to_packet.I.clock
        ; clear
        ; in_valid = serial_buffer_valid
        ; in_data = serial_buffer_data
        ; dn = { tready = router_ready }
        }
    in
    let serial_to_packet_valid = dn.tvalid in
    serial_to_packet_ready <-- serial_to_packet_ready';
    let dma_ready = wire 1 in
    let pulse_ready = wire 1 in
    let router =
      Router.hierarchical
        scope
        { Router.I.clock
        ; clear
        ; up = dn
        ; dns = [ { tready = dma_ready }; { tready = pulse_ready } ]
        }
    in
    router_ready <-- router.up.tready;
    let dma =
      Dma.hierarchical
        ~instance:"dma"
        scope
        { Dma.I.clock
        ; clear
        ; in_ = List.nth_exn router.dns 0
        ; out = write_bus
        ; out_ack = write_response
        }
    in
    dma_ready <-- dma.in_.tready;
    let pulse =
      Pulse.hierarchical scope { Pulse.I.clock; clear; up = List.nth_exn router.dns 1 }
    in
    pulse_ready <-- pulse.up.tready;
    let tx_enable = Tx_input.With_valid.Of_signal.wires () in
    let uart_tx_ready = wire 1 in
    let dma_out =
      Memory_to_packet8.hierarchical
        scope
        { Memory_to_packet8.I.clock
        ; clear
        ; enable = tx_enable
        ; memory = read_bus
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
    Some
      { write_request = dma.out
      ; read_request = dma_out.memory
      ; read_response
      ; write_response
      ; write_bus
      ; read_bus
      ; tx_input = tx_enable
      ; tx_busy = dma_out.busy
      ; uart_tx = dma_out_uart_tx.uart_tx
      ; uart_rx_valid
      ; parity_error
      ; serial_to_packet_valid
      ; clear_message = pulse.signal
      }
  ;;

  let maybe_dma_controller ~uart_rx ~clock ~clear scope =
    match General_config.include_io_controller with
    | No_io_controller -> None
    | Uart_controller config ->
      dma_controller ~config ~uart_rx:(Option.value_exn uart_rx) ~clock ~clear scope
  ;;
end
