open! Core
open Hardcaml
open Hardcaml_memory_controller
open Hardcaml_io_controller
open Hardcaml_uart_controller
open Signal
open Always

module Make (General_config : Cpu_intf.Config) (Memory : Memory_bus_intf.S) = struct
  module Memory_to_packet8 =
    Memory_to_packet8.Make
      (struct
        let header = Some 'D'
      end)
      (Memory)

  module Tx_input = Memory_to_packet8.Input

  type 'a t =
    { dma_to_memory_controller : Memory.Tx_bus.Tx.Of_signal.t list
    ; dma_to_memory_controller_rx : Variable.t Memory.Tx_bus.Rx.t list
    ; memory_controller_to_dma : Variable.t Memory.Rx_bus.Tx.t list
    ; memory_controller_to_dma_rx : Memory.Rx_bus.Rx.Of_signal.t list
    ; uart_rx_valid : 'a
    ; tx_input : Signal.t Tx_input.With_valid.t
    ; tx_busy : 'a
    ; uart_tx : Signal.t
    ; parity_error : 'a
    ; stop_bit_unstable : 'a
    ; serial_to_packet_valid : 'a
    ; clear_message : 'a
    }
  [@@deriving fields]

  let dma_controller ~config ~uart_rx ~clock ~clear scope =
    let module Config = struct
      let config = config
    end
    in
    let module Packet =
      Packet.Make (struct
        let data_bus_width = 8
      end)
    in
    let module Dma = Packet_to_memory.Make (Memory) (Packet) in
    let module Uart_tx = Uart_tx.Make (Config) in
    let module Uart_rx = Uart_rx.Make (Config) in
    let module Serial_to_packet =
      Serial_to_packet.Make
        (struct
          let header = 'Q'
          let serial_input_width = 8
          let max_packet_length_in_data_widths = 16
        end)
        (Packet)
    in
    let module Router =
      Router.Make
        (struct
          let num_tags = 2
        end)
        (Packet)
    in
    let module Pulse = Pulse.Make (Packet) in
    let reg_spec_no_clear = Reg_spec.create ~clock () in
    let rx_dma_to_memory_controller = Memory.Tx_bus.Rx.Of_always.wire zero in
    let rx_memory_controller_to_dma = Memory.Rx_bus.Tx.Of_always.wire zero in
    let { Uart_rx.O.data_out_valid; data_out; parity_error; stop_bit_unstable } =
      Uart_rx.hierarchical ~instance:"rx" scope { Uart_rx.I.clock; clear; uart_rx }
    in
    let router_ready = wire 1 in
    let { Serial_to_packet.O.out } =
      Serial_to_packet.hierarchical
        ~instance:"serial_to_packet"
        scope
        { Serial_to_packet.I.clock
        ; clear
        ; in_valid = data_out_valid
        ; in_data = data_out
        ; out = { ready = vdd }
        }
    in
    let dma_ready = wire 1 in
    let pulse_ready = wire 1 in
    let router =
      Router.hierarchical
        ~instance:"io_packet_router"
        scope
        { Router.I.clock
        ; clear
        ; in_ = out
        ; outs = [ { ready = dma_ready }; { ready = pulse_ready } ]
        }
    in
    router_ready <== router.in_.ready;
    let dma =
      Dma.hierarchical
        ~instance:"dma"
        scope
        { Dma.I.clock
        ; clear
        ; in_ = List.nth_exn router.outs 0
        ; out = Memory.Tx_bus.Rx.Of_always.value rx_dma_to_memory_controller
        ; out_ack = Memory.Rx_bus.Tx.Of_always.value rx_memory_controller_to_dma
        }
    in
    dma_ready <== dma.in_.ready;
    let pulse =
      Pulse.hierarchical
        ~instance:"pulse"
        scope
        { Pulse.I.clock; clear; in_ = List.nth_exn router.outs 1 }
    in
    pulse_ready <== pulse.in_.ready;
    let tx_enable = Tx_input.With_valid.Of_signal.wires () in
    let tx_dma_to_memory_controller = Memory.Tx_bus.Rx.Of_always.wire zero in
    let tx_memory_controller_to_dma = Memory.Rx_bus.Tx.Of_always.wire zero in
    let uart_tx_ready = wire 1 in
    let dma_out =
      Memory_to_packet8.hierarchical
        ~instance:"dma_out"
        scope
        { Memory_to_packet8.I.clock
        ; clear
        ; enable = tx_enable
        ; memory = Memory.Tx_bus.Rx.Of_always.value tx_dma_to_memory_controller
        ; memory_response = Memory.Rx_bus.Tx.Of_always.value tx_memory_controller_to_dma
        ; output_packet = { ready = uart_tx_ready }
        }
    in
    let dma_out_uart_tx =
      Uart_tx.hierarchical
        ~instance:"tx"
        scope
        { Uart_tx.I.clock
        ; clear
        ; data_in_valid = dma_out.output_packet.valid
        ; data_in = dma_out.output_packet.data.data
        }
    in
    uart_tx_ready <== dma_out_uart_tx.data_in_ready;
    let pulse_held =
      reg_fb
        ~width:8
        ~f:(fun t ->
          mux2 pulse.signal (of_int ~width:8 255) (mux2 (t ==:. 0) (zero 8) (t -:. 1)))
        reg_spec_no_clear
    in
    Some
      { dma_to_memory_controller = [ dma.out; dma_out.memory ]
      ; dma_to_memory_controller_rx =
          [ rx_dma_to_memory_controller; tx_dma_to_memory_controller ]
      ; memory_controller_to_dma =
          [ rx_memory_controller_to_dma; tx_memory_controller_to_dma ]
      ; memory_controller_to_dma_rx = [ dma.out_ack; dma_out.memory_response ]
      ; tx_input = tx_enable
      ; tx_busy = dma_out.busy
      ; uart_tx = dma_out_uart_tx.uart_tx
      ; uart_rx_valid = data_out_valid
      ; parity_error
      ; stop_bit_unstable
      ; serial_to_packet_valid = out.valid
      ; clear_message = pulse_held <>:. 0
      }
  ;;

  let maybe_dma_controller ~uart_rx ~clock ~clear scope =
    match General_config.include_io_controller with
    | No_io_controller -> None
    | Uart_controller config -> dma_controller ~config ~uart_rx ~clock ~clear scope
  ;;
end
