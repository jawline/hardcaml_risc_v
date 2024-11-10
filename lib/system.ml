open! Core
open Hardcaml
open Hardcaml_memory_controller
open Hardcaml_risc_v_hart
open Signal
open Always

module Make
    (Hart_config : Hart_config_intf.S)
    (Memory_config : System_intf.Memory_config)
    (General_config : System_intf.Config) =
struct
  let register_width = Register_width.bits Hart_config.register_width

  let system_non_hart_memory_channels =
    match General_config.include_io_controller with
    | No_io_controller -> 0
    | Uart_controller _ -> 1
  ;;

  module Memory_controller = Memory_controller.Make (struct
      let capacity_in_bytes = Memory_config.num_bytes
      let num_read_channels = system_non_hart_memory_channels + General_config.num_harts
      let num_write_channels = system_non_hart_memory_channels + General_config.num_harts
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

  module Dma = System_dma_controller.Make (General_config) (Memory_controller.Memory_bus)
  module Video_out = Video_out.Make (Memory_controller.Memory_bus)

  let include_uart_wires =
    match General_config.include_io_controller with
    | Uart_controller _ -> true
    | _ -> false
  ;;

  let include_video_out =
    match General_config.include_video_out with
    | No_video_out -> false
    | Video_out _ -> true
  ;;

  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; uart_rx : 'a option [@exists include_uart_wires]
      ; video_in : 'a Video_out.Screen_signals.t option [@exists include_video_out]
      }
    [@@deriving hardcaml ~rtlmangle:"$"]
  end

  module O = struct
    type 'a t =
      { registers : 'a Registers.t list [@length General_config.num_harts]
      ; uart_tx : 'a option [@exists include_uart_wires]
      ; uart_rx_valid : 'a option [@exists include_uart_wires]
      ; parity_error : 'a option [@exists include_uart_wires]
      ; stop_bit_unstable : 'a option [@exists include_uart_wires]
      ; serial_to_packet_valid : 'a option [@exists include_uart_wires]
      ; video_out : 'a Video_out.Video_data.t option [@exists include_video_out]
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
    ~clock
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
    let reg_spec_no_clear = Reg_spec.create ~clock () in
    let hart0 = List.nth_exn harts 0 in
    (* Delay the ecall by a cycle so we can register all the relevant
     * registers, reducing routing pressure. *)
    (* TODO: This is pretty jank - move ecall to write back and pass 'ecall registers' out of the pipeline *)
    let%hw delayed_r5 = reg reg_spec_no_clear (List.nth_exn hart0.registers.general 5) in
    let%hw delayed_r6 = reg reg_spec_no_clear (List.nth_exn hart0.registers.general 6) in
    let%hw delayed_r7 = reg reg_spec_no_clear (List.nth_exn hart0.registers.general 7) in
    let%hw is_dma_write = hart0.is_ecall &: (delayed_r5 ==:. 0) in
    let%hw next_pc = reg reg_spec_no_clear (hart0.registers.pc +:. 4) in
    (* TODO: I think this can race. *)
    let not_busy = ~:tx_busy in
    Dma.Tx_input.With_valid.Of_signal.(
      tx_input
      <== { valid = is_dma_write &: not_busy
          ; value =
              { address = delayed_r6 -- "dma$address"
              ; length =
                  uresize ~width:Dma.Tx_input.port_widths.length delayed_r7
                  -- "dma$length"
              }
          });
    (* Assign the Hart0 transaction. *)
    Transaction.Of_signal.(
      List.hd_exn hart_ecall_transactions
      <== { Transaction.set_rd = vdd
          ; new_rd = uresize ~width:register_width (is_dma_write &: not_busy)
          ; new_pc = next_pc
          ; error = gnd
          });
    (* Default the remainders *)
    List.zip_exn harts hart_ecall_transactions
    |> List.tl_exn
    |> List.iter ~f:assign_non_io_ecall
  ;;

  let assign_ecalls ~clock maybe_dma_controller harts hart_ecall_transactions scope =
    (* If a DMA controller is in the design then wire up DMA related ecalls
       otherwise do not include any ecalls *)
    match maybe_dma_controller with
    | Some config -> assign_dma_io_ecall ~clock harts hart_ecall_transactions config scope
    | None -> assign_empty_ecalls harts hart_ecall_transactions
  ;;

  module Video_data = struct
    type 'a t =
      { video_data : 'a Video_out.Video_data.t
      ; memory_request_ack : 'a Memory_controller.Memory_bus.Read_bus.Rx.t
      ; memory_request : 'a Memory_controller.Memory_bus.Read_bus.Tx.t
      ; memory_response : 'a Memory_controller.Memory_bus.Read_response.With_valid.t
      }
    [@@deriving fields ~getters]
  end

  let maybe_video_out scope (i : _ I.t) =
    match General_config.include_video_out with
    | No_video_out -> None
    | Video_out
        { output_width : int
        ; output_height : int
        ; framebuffer_width : int
        ; framebuffer_height : int
        } ->
      let module Config = struct
        let input_width = framebuffer_width
        let input_height = framebuffer_height
        let output_width = output_width
        let output_height = output_height
        let framebuffer_address = 0x8000
      end
      in
      let memory_request_ack =
        Memory_controller.Memory_bus.Read_bus.Rx.Of_signal.wires ()
      in
      let memory_response =
        Memory_controller.Memory_bus.Read_response.With_valid.Of_signal.wires ()
      in
      let video_out =
        Video_out.hierarchical
          ~config:(module Config)
          scope
          { Video_out.I.clock = i.clock
          ; clear = i.clear
          ; screen = Option.value_exn i.video_in
          ; memory_request = memory_request_ack
          ; memory_response
          }
      in
      Some
        { Video_data.video_data = video_out.video_data
        ; memory_request_ack
        ; memory_request = video_out.memory_request
        ; memory_response
        }
  ;;

  let create scope (i : _ I.t) =
    (* If the design has requested a video out then initialize it. *)
    let maybe_video_out = maybe_video_out scope i in
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
    let of_dma ~f = Option.map ~f maybe_dma_controller in
    let of_video_out ~f = Option.map ~f maybe_video_out in
    let controller =
      Memory_controller.hierarchical
        ~instance:"Memory_controller"
        scope
        { Memory_controller.I.clock = i.clock
        ; clear = i.clear
        ; read_to_controller =
            (of_video_out ~f:(fun vd -> [ vd.memory_request ]) |> Option.value ~default:[])
            @ (of_dma ~f:(fun dma -> [ dma.read_request ]) |> Option.value ~default:[])
            @ List.map ~f:Read_bus.Tx.Of_always.value read_bus_per_hart
        ; write_to_controller =
            (of_dma ~f:(fun dma -> [ dma.write_request ]) |> Option.value ~default:[])
            @ List.map ~f:Write_bus.Tx.Of_always.value write_bus_per_hart
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
                  of_dma ~f:Dma.clear_message |> Option.value ~default:gnd |: i.clear
              ; read_bus =
                  List.nth_exn
                    controller.read_to_controller
                    (system_non_hart_memory_channels + which_hart)
              ; write_bus =
                  List.nth_exn
                    controller.write_to_controller
                    (system_non_hart_memory_channels + which_hart)
              ; read_response =
                  List.nth_exn
                    controller.read_response
                    (system_non_hart_memory_channels + which_hart)
              ; write_response =
                  List.nth_exn
                    controller.write_response
                    (system_non_hart_memory_channels + which_hart)
              ; ecall_transaction = List.nth_exn hart_ecall_transactions which_hart
              }
          in
          hart)
        General_config.num_harts
    in
    assign_ecalls ~clock:i.clock maybe_dma_controller harts hart_ecall_transactions scope;
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
    { O.registers = List.map ~f:(fun o -> o.registers) harts
    ; uart_tx = of_dma ~f:Dma.uart_tx
    ; uart_rx_valid = of_dma ~f:Dma.uart_rx_valid
    ; parity_error = of_dma ~f:Dma.parity_error
    ; stop_bit_unstable = of_dma ~f:Dma.stop_bit_unstable
    ; serial_to_packet_valid = of_dma ~f:Dma.serial_to_packet_valid
    ; video_out = of_video_out ~f:Video_data.video_data
    }
  ;;

  let hierarchical ~instance (scope : Scope.t) (input : Signal.t I.t) =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ~scope ~name:"cpu" ~instance create input
  ;;
end
