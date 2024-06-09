open! Core
open Hardcaml
open Hardcaml_memory_controller
open Hardcaml_io_controller
open Hardcaml_uart_controller
open Hardcaml_risc_v_hart
open Signal
open Always
(* TODO: Add an enable? *)

module Make
    (Hart_config : Hart_config_intf.S)
    (Memory_config : sig
       val num_bytes : int
     end)
    (General_config : sig
       val num_harts : int
       val include_io_controller : Io_controller_config.t
     end) =
struct
  let system_non_hart_memory_channels =
    match General_config.include_io_controller with
    | No_io_controller -> 0
    | Uart_controller _ -> 2
  ;;

  module Memory_controller = Memory_controller.Make (struct
      let num_bytes = Memory_config.num_bytes
      let num_channels = system_non_hart_memory_channels + General_config.num_harts
      let address_width = Address_width.bits Hart_config.address_width
      let data_bus_width = 32
    end)

  module Registers = Registers.Make (Hart_config)
  module Decoded_instruction = Decoded_instruction.Make (Hart_config) (Registers)
  module Transaction = Transaction.Make (Hart_config) (Memory_controller)

  module Hart =
    Hart.Make (Hart_config) (Memory_controller) (Registers) (Decoded_instruction)
      (Transaction)

  module Memory_to_packet8 =
    Memory_to_packet8.Make
      (struct
        let magic = Some 'D'
      end)
      (Memory_controller)

  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; (* These are optional if include_io_controller = Uart_io. *)
        (* TODO: Remove these from the I.t when unused *)
        uart_rx : 'a
      }
    [@@deriving sexp_of, hardcaml ~rtlmangle:true]
  end

  module O = struct
    type 'a t =
      { registers : 'a Registers.t list [@length General_config.num_harts]
      ; uart_tx : 'a
      }
    [@@deriving sexp_of, hardcaml ~rtlmangle:true]
  end

  module Tx_input = Memory_to_packet8.Input

  module Dma_wiring = struct
    type 'a t =
      { dma_to_memory_controller : Memory_controller.Tx_bus.Tx.Of_signal.t list
      ; dma_to_memory_controller_rx : Variable.t Memory_controller.Tx_bus.Rx.t list
      ; memory_controller_to_dma : Variable.t Memory_controller.Rx_bus.Tx.t list
      ; memory_controller_to_dma_rx : Memory_controller.Rx_bus.Rx.Of_signal.t list
      ; tx_input : Signal.t Tx_input.With_valid.t
      ; tx_busy : 'a
      ; uart_tx : Signal.t
      }
  end

  let maybe_dma_controller ~uart_rx ~clock ~clear scope =
    match General_config.include_io_controller with
    | No_io_controller -> None
    | Uart_controller config ->
      let module Config = struct
        let config = config
      end
      in
      let module Packet =
        Packet.Make (struct
          let data_bus_width = 8
        end)
      in
      let module Dma = Packet_to_memory.Make (Memory_controller) (Packet) in
      let module Uart_tx = Uart_tx.Make (Config) in
      let module Uart_rx = Uart_rx.Make (Config) in
      let module Serial_to_packet =
        Serial_to_packet.Make
          (struct
            let magic = 'Q'
            let serial_input_width = 8
            let max_packet_length_in_data_widths = 16
          end)
          (Packet)
      in
      let rx_dma_to_memory_controller = Memory_controller.Tx_bus.Rx.Of_always.wire zero in
      let rx_memory_controller_to_dma = Memory_controller.Rx_bus.Tx.Of_always.wire zero in
      let { Uart_rx.O.data_out_valid; data_out; parity_error = _; stop_bit_unstable = _ } =
        Uart_rx.hierarchical ~instance:"rx" scope { Uart_rx.I.clock; clear; uart_rx }
      in
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
      let dma =
        Dma.hierarchical
          ~instance:"dma"
          scope
          { Dma.I.clock
          ; clear
          ; in_ = out
          ; out = Memory_controller.Tx_bus.Rx.Of_always.value rx_dma_to_memory_controller
          ; out_ack =
              Memory_controller.Rx_bus.Tx.Of_always.value rx_memory_controller_to_dma
          }
      in
      let tx_enable = Tx_input.With_valid.Of_signal.wires () in
      let ( -- ) = Scope.naming scope in
      ignore (tx_enable.valid -- "tx_enable" : Signal.t);
      let tx_dma_to_memory_controller = Memory_controller.Tx_bus.Rx.Of_always.wire zero in
      let tx_memory_controller_to_dma = Memory_controller.Rx_bus.Tx.Of_always.wire zero in
      let uart_tx_ready = wire 1 in
      let dma_out =
        Memory_to_packet8.hierarchical
          ~instance:"dma_out"
          scope
          { Memory_to_packet8.I.clock
          ; clear
          ; enable = tx_enable
          ; memory =
              Memory_controller.Tx_bus.Rx.Of_always.value tx_dma_to_memory_controller
          ; memory_response =
              Memory_controller.Rx_bus.Tx.Of_always.value tx_memory_controller_to_dma
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
      Some
        { Dma_wiring.dma_to_memory_controller = [ dma.out; dma_out.memory ]
        ; dma_to_memory_controller_rx =
            [ rx_dma_to_memory_controller; tx_dma_to_memory_controller ]
        ; memory_controller_to_dma =
            [ rx_memory_controller_to_dma; tx_memory_controller_to_dma ]
        ; memory_controller_to_dma_rx = [ dma.out_ack; dma_out.memory_response ]
        ; tx_input = tx_enable
        ; tx_busy = dma_out.busy
        ; uart_tx = dma_out_uart_tx.uart_tx
        }
  ;;

  let create scope (i : _ I.t) =
    (* If the design has requested a DMA controller then initialize it. *)
    let maybe_dma_controller =
      maybe_dma_controller ~uart_rx:i.uart_rx ~clock:i.clock ~clear:i.clear scope
    in
    let ch_to_controller_per_hart =
      List.init
        ~f:(fun _which_hart -> Memory_controller.Tx_bus.Tx.Of_always.wire zero)
        General_config.num_harts
    in
    let controller_to_ch_per_hart =
      List.init
        ~f:(fun _which_hart -> Memory_controller.Rx_bus.Rx.Of_always.wire zero)
        General_config.num_harts
    in
    let controller =
      Memory_controller.hierarchical
        ~instance:"Memory_controller"
        scope
        { Memory_controller.I.clock = i.clock
        ; clear = i.clear
        ; ch_to_controller =
            (match maybe_dma_controller with
             | None -> []
             | Some { dma_to_memory_controller; _ } -> dma_to_memory_controller)
            @ List.map
                ~f:Memory_controller.Tx_bus.Tx.Of_always.value
                ch_to_controller_per_hart
        ; controller_to_ch =
            (match maybe_dma_controller with
             | None -> []
             | Some { memory_controller_to_dma_rx; _ } -> memory_controller_to_dma_rx)
            @ List.map
                ~f:Memory_controller.Rx_bus.Rx.Of_always.value
                controller_to_ch_per_hart
        }
    in
    let harts =
      List.init
        ~f:(fun which_hart ->
          Hart.hierarchical
            ~custom_ecall:
              (fun
                ~is_ecall ~decoded_instruction:_ ~(registers : Signal.t Registers.t) ->
              match maybe_dma_controller with
              | Some { tx_input; tx_busy; _ } ->
                let should_do_dma =
                  is_ecall &: (List.nth_exn registers.general 1 ==:. 0)
                in
                let not_busy = ~:tx_busy in
                tx_input.valid <== (should_do_dma &: not_busy);
                tx_input.value.address <== List.nth_exn registers.general 2;
                tx_input.value.length
                <== uresize
                      (List.nth_exn registers.general 3)
                      (width tx_input.value.length);
                { Transaction.finished = vdd
                ; set_rd = vdd
                ; new_rd = uresize not_busy 32
                ; new_pc = registers.pc +:. 4
                ; error = zero 1
                }
              | None ->
                (* Set RD to false (failure) and do nothing else. *)
                { Transaction.finished = vdd
                ; set_rd = vdd
                ; new_rd = zero 32
                ; new_pc = registers.pc +:. 4
                ; error = gnd
                })
            ~instance:[%string "hart_%{which_hart#Int}"]
            scope
            { Hart.I.clock = i.clock
            ; clear = i.clear
            ; memory_controller_to_hart =
                List.nth_exn
                  controller.controller_to_ch
                  (system_non_hart_memory_channels + which_hart)
            ; hart_to_memory_controller =
                List.nth_exn
                  controller.ch_to_controller
                  (system_non_hart_memory_channels + which_hart)
            })
        General_config.num_harts
    in
    (* TODO: After adding the DMA controller this code has got a little ugly. Factor it out? *)
    compile
      ([ List.map
           ~f:(fun (hart, ch_to_controller_per_hart) ->
             Memory_controller.Tx_bus.Tx.Of_always.assign
               ch_to_controller_per_hart
               hart.hart_to_memory_controller)
           (List.zip_exn
              harts
              (List.take ch_to_controller_per_hart General_config.num_harts))
         |> proc
       ; List.map
           ~f:(fun (hart, controller_to_ch_per_hart) ->
             Memory_controller.Rx_bus.Rx.Of_always.assign
               controller_to_ch_per_hart
               hart.memory_controller_to_hart)
           (List.zip_exn
              harts
              (List.take controller_to_ch_per_hart General_config.num_harts))
         |> proc
       ]
       @
       match maybe_dma_controller with
       | None -> []
       | Some { dma_to_memory_controller_rx; memory_controller_to_dma; _ } ->
         List.mapi
           ~f:(fun i t ->
             Memory_controller.Tx_bus.Rx.Of_always.assign
               t
               (List.nth_exn controller.ch_to_controller i))
           dma_to_memory_controller_rx
         @ List.mapi
             ~f:(fun i t ->
               Memory_controller.Rx_bus.Tx.Of_always.assign
                 t
                 (List.nth_exn controller.controller_to_ch i))
             memory_controller_to_dma);
    { O.registers = List.map ~f:(fun o -> o.registers) harts
    ; uart_tx =
        (match maybe_dma_controller with
         | Some { uart_tx; _ } -> uart_tx
         | None -> gnd)
    }
  ;;

  let hierarchical ~instance (scope : Scope.t) (input : Signal.t I.t) =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ~scope ~name:"Cpu" ~instance create input
  ;;
end
