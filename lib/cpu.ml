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
  let register_width = Register_width.bits Hart_config.register_width

  let system_non_hart_memory_channels =
    match General_config.include_io_controller with
    | No_io_controller -> 0
    | Uart_controller _ -> 1
  ;;

  let hart_required_read_channels = Hart.required_read_channels
  let hart_required_write_channels = Hart.required_write_channels

  module Memory_controller = Memory_controller.Make (struct
      let capacity_in_bytes = Memory_config.num_bytes

      let num_read_channels =
        system_non_hart_memory_channels
        + (General_config.num_harts * hart_required_read_channels)
      ;;

      let num_write_channels =
        system_non_hart_memory_channels
        + (General_config.num_harts * hart_required_write_channels)
      ;;

      let address_width = Register_width.bits Hart_config.register_width
      let data_bus_width = 32
    end)

  open Memory_controller.Memory_bus
  module Registers = Registers.Make (Hart_config)
  module Decoded_instruction = Decoded_instruction.Make (Hart_config) (Registers)
  module Transaction = Transaction.Make (Hart_config) (Memory_controller.Memory_bus)

  module Hart =
    Hart.Make (Hart_config) (Memory_controller.Memory_bus) (Registers)
      (Decoded_instruction)
      (Transaction)

  module Dma = Cpu_dma_controller.Make (General_config) (Memory_controller.Memory_bus)

  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; (* ignored if include_io_controller = Uart_io. *)
        uart_rx : 'a
      }
    [@@deriving hardcaml ~rtlmangle:"$"]
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
    [@@deriving hardcaml ~rtlmangle:"$"]
  end

  let default_transaction (hart : _ Hart.O.t) =
    { Transaction.set_rd = vdd
    ; new_rd = zero register_width
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
    scope
    =
    (* For now we only allow Hart0 to do IO. This isn't
       necessary, but it makes it easier to stop them both
       issuing commands at the same time and entering a weird
       state. *)
    let ( -- ) = Scope.naming scope in
    let hart0 = List.nth_exn harts 0 in
    let should_do_dma =
      hart0.is_ecall &: (List.nth_exn hart0.registers.general 5 ==:. 0)
    in
    let not_busy = ~:tx_busy in
    Dma.Tx_input.With_valid.Of_signal.(
      tx_input
      <== { valid = should_do_dma &: not_busy
          ; value =
              { address = List.nth_exn hart0.registers.general 6 -- "dma$address"
              ; length =
                  (let%hw dma_length =
                     uresize
                       ~width:(width tx_input.value.length)
                       (List.nth_exn hart0.registers.general 7)
                   in
                   dma_length)
              }
          });
    (* Assign the Hart0 transaction *)
    Transaction.Of_signal.(
      List.hd_exn hart_ecall_transactions
      <== { Transaction.set_rd = vdd
          ; new_rd = uresize ~width:register_width not_busy
          ; new_pc = hart0.registers.pc +:. 4
          ; error = gnd
          });
    (* Default the remainders *)
    List.zip_exn harts hart_ecall_transactions
    |> List.tl_exn
    |> List.iter ~f:assign_non_io_ecall
  ;;

  let assign_ecalls maybe_dma_controller harts hart_ecall_transactions scope =
    (* If a DMA controller is in the design then wire up DMA related ecalls
       otherwise do not include any ecalls *)
    match maybe_dma_controller with
    | Some config -> assign_dma_io_ecall harts hart_ecall_transactions config scope
    | None -> assign_empty_ecalls harts hart_ecall_transactions
  ;;

  let create scope (i : _ I.t) =
    (* If the design has requested a DMA controller then initialize it. *)
    let maybe_dma_controller =
      Dma.maybe_dma_controller ~uart_rx:i.uart_rx ~clock:i.clock ~clear:i.clear scope
    in
    let read_bus_per_hart =
      List.init
        ~f:(fun _which_hart -> Read_bus.Tx.Of_always.wire zero)
        General_config.num_harts
    in
    let write_bus_per_hart =
      List.init
        ~f:(fun _which_hart -> Write_bus.Tx.Of_always.wire zero)
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
        ; read_to_controller =
            of_dma ~default:[] ~f:(fun dma -> [ dma.read_request ])
            @ List.map ~f:Read_bus.Tx.Of_always.value read_bus_per_hart
        ; write_to_controller =
            of_dma ~default:[] ~f:(fun dma -> [ dma.write_request ])
            @ List.map ~f:Write_bus.Tx.Of_always.value write_bus_per_hart
        }
    in
    let hart_ecall_transactions =
      List.init ~f:(fun _ -> Transaction.Of_signal.wires ()) General_config.num_harts
    in
    let select_controller_parts ~mode ~(which_hart : int) t =
      let offset = system_non_hart_memory_channels in
      let stride =
        match mode with
        | `Read -> hart_required_read_channels
        | `Write -> hart_required_write_channels
      in
      let dropped = List.drop t (offset + (which_hart * stride)) in
      List.take dropped stride
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
              ; read_bus =
                  select_controller_parts
                    ~mode:`Read
                    ~which_hart
                    controller.read_to_controller
              ; write_bus =
                  select_controller_parts
                    ~mode:`Write
                    ~which_hart
                    controller.write_to_controller
              ; read_response =
                  select_controller_parts ~mode:`Read ~which_hart controller.read_response
              ; write_response =
                  select_controller_parts
                    ~mode:`Write
                    ~which_hart
                    controller.write_response
              ; ecall_transaction = List.nth_exn hart_ecall_transactions which_hart
              }
          in
          hart)
        General_config.num_harts
    in
    assign_ecalls maybe_dma_controller harts hart_ecall_transactions scope;
    compile
      (List.map
         ~f:(fun (hart, read_bus) -> Read_bus.Tx.Of_always.assign read_bus hart.read_bus)
         (List.zip_exn harts read_bus_per_hart)
       @ List.map
           ~f:(fun (hart, write_bus) ->
             Write_bus.Tx.Of_always.assign write_bus hart.write_bus)
           (List.zip_exn harts write_bus_per_hart)
       @
       match maybe_dma_controller with
       | None -> []
       | Some { read_bus; write_bus; read_response; write_response; _ } ->
         [ Read_bus.Rx.Of_always.assign
             read_bus
             (List.nth_exn controller.read_to_controller 0)
         ; Write_bus.Rx.Of_always.assign
             write_bus
             (List.nth_exn controller.write_to_controller 0)
         ; Read_response.With_valid.Of_always.assign
             read_response
             (List.nth_exn controller.read_response 0)
         ; Write_response.With_valid.Of_always.assign
             write_response
             (List.nth_exn controller.write_response 0)
         ]);
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
