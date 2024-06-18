open! Core
open Hardcaml
open Hardcaml_memory_controller
open Hardcaml_risc_v_hart
open Signal
open Always

module Make
    (Hart_config : Hart_config_intf.S)
    (Memory_config : Cpu_intf.Memory_config)
    (General_config : Cpu_intf.Config) =
struct
  let system_non_hart_memory_channels =
    match General_config.include_io_controller with
    | No_io_controller -> 0
    | Uart_controller _ -> 2
  ;;

  module Memory_controller = Memory_controller.Make (struct
      let num_bytes = Memory_config.num_bytes
      let num_channels = system_non_hart_memory_channels + General_config.num_harts
      let address_width = Register_width.bits Hart_config.register_width
      let data_bus_width = 32
    end)

  module Registers = Registers.Make (Hart_config)
  module Decoded_instruction = Decoded_instruction.Make (Hart_config) (Registers)
  module Transaction = Transaction.Make (Hart_config) (Memory_controller)

  module Hart =
    Hart.Make (Hart_config) (Memory_controller) (Registers) (Decoded_instruction)
      (Transaction)

  module Dma = Cpu_dma_controller.Make (General_config) (Memory_controller)

  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; (* ignored if include_io_controller = Uart_io. *)
        uart_rx : 'a
      }
    [@@deriving sexp_of, hardcaml ~rtlmangle:true]
  end

  module O = struct
    type 'a t =
      { registers : 'a Registers.t list [@length General_config.num_harts]
      ; (* gnd if include_io_controller = Uart_io. *)
        uart_tx : 'a
      ; uart_rx_valid : 'a
      ; parity_error : 'a
      ; stop_bit_unstable : 'a
      ; serial_to_packet_valid : 'a
      }
    [@@deriving sexp_of, hardcaml ~rtlmangle:true]
  end

  let default_transaction (hart : _ Hart.O.t) =
    { Transaction.finished = vdd
    ; set_rd = vdd
    ; new_rd = zero 32
    ; new_pc = hart.registers.pc +:. 4
    ; error = gnd
    }
  ;;

  let assign_non_io_ecall ((hart : _ Hart.O.t), transaction) =
    (* Default the remainders *)
    Transaction.Of_signal.(transaction <== default_transaction hart)
  ;;

  let assign_empty_ecalls harts hart_ecall_transactions =
    List.zip_exn harts hart_ecall_transactions |> List.iter ~f:assign_non_io_ecall
  ;;

  let assign_dma_io_ecall
    (harts : _ Hart.O.t list)
    hart_ecall_transactions
    { Dma.tx_input; tx_busy; _ }
    =
    (* For now we only allow Hart0 to do IO. This isn't
       necessary, but it makes it easier to stop them both
       issuing commands at the same time and entering a weird
       state. *)
    let hart0 = List.nth_exn harts 0 in
    let should_do_dma =
      hart0.is_ecall &: (List.nth_exn hart0.registers.general 5 ==:. 0)
    in
    let not_busy = ~:tx_busy in
    Dma.Tx_input.With_valid.Of_signal.(
      tx_input
      <== { valid = should_do_dma &: not_busy
          ; value =
              { address = List.nth_exn hart0.registers.general 6
              ; length =
                  uresize
                    (List.nth_exn hart0.registers.general 7)
                    (width tx_input.value.length)
              }
          });
    (* Assign the Hart0 transaction *)
    Transaction.Of_signal.(
      List.hd_exn hart_ecall_transactions
      <== { Transaction.finished = vdd
          ; set_rd = vdd
          ; new_rd = uresize not_busy 32
          ; new_pc = hart0.registers.pc +:. 4
          ; error = zero 1
          });
    (* Default the remainders *)
    List.zip_exn harts hart_ecall_transactions
    |> List.tl_exn
    |> List.iter ~f:assign_non_io_ecall
  ;;

  let assign_ecalls maybe_dma_controller harts hart_ecall_transactions =
    (* If a DMA controller is in the design then wire up DMA related ecalls
       otherwise do not include any ecalls *)
    match maybe_dma_controller with
    | Some config -> assign_dma_io_ecall harts hart_ecall_transactions config
    | None -> assign_empty_ecalls harts hart_ecall_transactions
  ;;

  let create scope (i : _ I.t) =
    (* If the design has requested a DMA controller then initialize it. *)
    let maybe_dma_controller =
      Dma.maybe_dma_controller ~uart_rx:i.uart_rx ~clock:i.clock ~clear:i.clear scope
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
    let of_dma ~default ~f =
      Option.map ~f maybe_dma_controller |> Option.value ~default
    in
    let controller =
      Memory_controller.hierarchical
        ~instance:"Memory_controller"
        scope
        { Memory_controller.I.clock = i.clock
        ; clear = i.clear
        ; ch_to_controller =
            of_dma ~default:[] ~f:Dma.dma_to_memory_controller
            @ List.map
                ~f:Memory_controller.Tx_bus.Tx.Of_always.value
                ch_to_controller_per_hart
        ; controller_to_ch =
            of_dma ~default:[] ~f:Dma.memory_controller_to_dma_rx
            @ List.map
                ~f:Memory_controller.Rx_bus.Rx.Of_always.value
                controller_to_ch_per_hart
        }
    in
    let hart_ecall_transactions =
      List.init ~f:(fun _ -> Transaction.Of_signal.wires ()) General_config.num_harts
    in
    let harts =
      List.init
        ~f:(fun which_hart ->
          let hart =
            Hart.hierarchical
              ~instance:[%string "hart_%{which_hart#Int}"]
              scope
              { Hart.I.clock = i.clock
              ; clear =
                  (* Allow resets via remote IO if a DMA controller is attached. *)
                  of_dma ~default:gnd ~f:Dma.clear_message |: i.clear
              ; memory_controller_to_hart =
                  List.nth_exn
                    controller.controller_to_ch
                    (system_non_hart_memory_channels + which_hart)
              ; hart_to_memory_controller =
                  List.nth_exn
                    controller.ch_to_controller
                    (system_non_hart_memory_channels + which_hart)
              ; ecall_transaction = List.nth_exn hart_ecall_transactions which_hart
              }
          in
          hart)
        General_config.num_harts
    in
    assign_ecalls maybe_dma_controller harts hart_ecall_transactions;
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
    let of_dma = of_dma ~default:gnd in
    { O.registers = List.map ~f:(fun o -> o.registers) harts
    ; uart_tx = of_dma ~f:Dma.uart_tx
    ; uart_rx_valid = of_dma ~f:Dma.uart_rx_valid
    ; parity_error = of_dma ~f:Dma.parity_error
    ; stop_bit_unstable = of_dma ~f:Dma.stop_bit_unstable
    ; serial_to_packet_valid = of_dma ~f:Dma.serial_to_packet_valid
    }
  ;;

  let hierarchical ~instance (scope : Scope.t) (input : Signal.t I.t) =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ~scope ~name:"cpu" ~instance create input
  ;;
end
