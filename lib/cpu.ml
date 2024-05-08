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
  module Memory_controller = Memory_controller.Make (struct
      let num_bytes = Memory_config.num_bytes

      let num_channels =
        General_config.num_harts
        +
        match General_config.include_io_controller with
        | No_io_controller -> 0
        | Uart_controller _ -> 1
      ;;

      let address_width = Address_width.bits Hart_config.address_width
      let data_bus_width = 32
    end)

  module Hart = Hart.Make (Hart_config) (Memory_controller)

  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; (* These are optional if include_io_controller = Uart_io. *)
        (* TODO: Remove these from the I.t when unused *)
        uart_rx : 'a
      ; uart_tx : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t =
      { registers : 'a Hart.Registers.t list [@length General_config.num_harts] }
    [@@deriving sexp_of, hardcaml]
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
      let module Dma = Dma_packet.Make (Memory_controller) (Packet) in
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
      let dma_to_memory_controller = Memory_controller.Tx_bus.Rx.Of_always.wire zero in
      let memory_controller_to_dma = Memory_controller.Rx_bus.Tx.Of_always.wire zero in
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
          ; out = { ready = one 1 }
          }
      in
      let dma =
        Dma.hierarchical
          ~instance:"dma"
          scope
          { Dma.I.clock
          ; clear
          ; in_ = out
          ; out = Memory_controller.Tx_bus.Rx.Of_always.value dma_to_memory_controller
          ; out_ack = Memory_controller.Rx_bus.Tx.Of_always.value memory_controller_to_dma
          }
      in
      Some (dma.out, dma.out_ack, dma_to_memory_controller, memory_controller_to_dma)
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
            (List.map
               ~f:Memory_controller.Tx_bus.Tx.Of_always.value
               ch_to_controller_per_hart
             @
             match maybe_dma_controller with
             | None -> []
             | Some (to_controller, _, _, _) -> [ to_controller ])
        ; controller_to_ch =
            (List.map
               ~f:Memory_controller.Rx_bus.Rx.Of_always.value
               controller_to_ch_per_hart
             @
             match maybe_dma_controller with
             | None -> []
             | Some (_, from_controller, _, _) -> [ from_controller ])
        }
    in
    let harts =
      List.init
        ~f:(fun which_hart ->
          Hart.hierarchical
            ~instance:[%string "hart_%{which_hart#Int}"]
            scope
            { Hart.I.clock = i.clock
            ; clear = i.clear
            ; memory_controller_to_hart =
                List.nth_exn controller.controller_to_ch which_hart
            ; hart_to_memory_controller =
                List.nth_exn controller.ch_to_controller which_hart
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
           (List.zip_exn harts (List.take ch_to_controller_per_hart General_config.num_harts))
         |> proc
       ; List.map
           ~f:(fun (hart, controller_to_ch_per_hart) ->
             Memory_controller.Rx_bus.Rx.Of_always.assign
               controller_to_ch_per_hart
               hart.memory_controller_to_hart)
           (List.zip_exn harts (List.take controller_to_ch_per_hart General_config.num_harts))
         |> proc
       ]
       @
       match maybe_dma_controller with
       | None -> []
       | Some (_, _, ch_to, to_ch) ->
         [ Memory_controller.Tx_bus.Rx.Of_always.assign
             ch_to
             (List.last_exn controller.ch_to_controller)
         ; Memory_controller.Rx_bus.Tx.Of_always.assign
             to_ch
             (List.last_exn controller.controller_to_ch)
         ]);
    { O.registers = List.map ~f:(fun o -> o.registers) harts }
  ;;

  let hierarchical ~instance (scope : Scope.t) (input : Signal.t I.t) =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ~scope ~name:"Cpu" ~instance create input
  ;;
end
