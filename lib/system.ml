open! Core
open Hardcaml
open Hardcaml_memory_controller
open Hardcaml_io_controller
open Hardcaml_risc_v_hart
open Signal

module Make
    (Hart_config : Hart_config_intf.S)
    (Memory_config : System_intf.Memory_config)
    (General_config : System_intf.Config)
    (Axi4 : Axi4.S) =
struct
  let () =
    if not (Custom_clock_domain.equal Hart_config.clock_domain General_config.dma_domain)
    then
      raise_s
        [%message
          "NOT IMPLEMENTED: The CPU does not currently support the DMA controller and \
           harts being on different memory domains"]
  ;;

  module Registers = Registers.Make (Hart_config)
  module Decoded_instruction = Decoded_instruction.Make (Hart_config) (Registers)

  let system_non_hart_read_memory_channels =
    (match General_config.include_io_controller with
     | No_io_controller -> 0
     | Uart_controller _ -> 1)
    +
    match General_config.include_video_out with
    | No_video_out -> 0
    | Video_out _ -> 1
  ;;

  let system_non_hart_write_memory_channels =
    match General_config.include_io_controller with
    | No_io_controller -> 0
    | Uart_controller _ -> 1
  ;;

  let num_instruction_read_channels = General_config.num_harts
  let num_instruction_write_channels = 0

  let num_data_read_channels =
    system_non_hart_read_memory_channels + General_config.num_harts
  ;;

  let num_data_write_channels =
    system_non_hart_write_memory_channels + General_config.num_harts
  ;;

  module Memory_controller =
    Memory_controller.Make
      (struct
        let capacity_in_bytes = Memory_config.capacity_in_bytes
        let address_width = Register_width.bits Hart_config.register_width
        let data_bus_width = Register_width.bits Hart_config.register_width

        module Instruction_config = struct
          let num_read_channels = num_instruction_read_channels
          let num_write_channels = num_instruction_write_channels

          let cache_memory =
            let open General_config in
            Option.map
              ~f:(fun (module Config : System_intf.Cache_config) ->
                (module struct
                  include Config

                  let num_read_channels = num_instruction_read_channels
                  let num_write_channels = num_instruction_write_channels
                end : Hardcaml_memory_controller.Axi4_cache.Config))
              include_instruction_cache
          ;;
        end

        module Data_config = struct
          let num_read_channels = num_data_read_channels
          let num_write_channels = num_data_write_channels

          let cache_memory =
            let open General_config in
            Option.map
              ~f:(fun (module Config : System_intf.Cache_config) ->
                (module struct
                  include Config

                  let num_read_channels = num_read_channels
                  let num_write_channels = num_write_channels
                end : Hardcaml_memory_controller.Axi4_cache.Config))
              include_data_cache
          ;;
        end
      end)
      (Axi4)
      (Axi4)

  module Memory_bus = Memory_controller.Memory_bus
  include Memory_bus

  let _instruction_ch_start_offset which_hart = which_hart
  let data_ch_start_offset which_hart = system_non_hart_read_memory_channels + which_hart

  let _select arr ch sz =
    let start = List.drop arr ch in
    List.take start sz
  ;;

  let select_instruction_rd_chs_for_hart which_hart arr = List.nth_exn arr which_hart

  let select_data_rd_chs_for_hart which_hart arr =
    List.nth_exn arr (data_ch_start_offset which_hart)
  ;;

  let select_data_wr_chs_for_hart which_hart arr =
    List.nth_exn arr (system_non_hart_write_memory_channels + which_hart)
  ;;

  module Transaction = Transaction.Make (Hart_config) (Memory_controller.Memory_bus)

  let register_width = Register_width.bits Hart_config.register_width

  module Hart =
    Hart.Make (Hart_config) (Memory_controller.Memory_bus) (Registers)
      (Decoded_instruction)
      (Transaction)

  module Memory_to_packet8 =
    Memory_to_packet8.Make
      (struct
        let header = Some 'D'
      end)
      (Memory_controller.Memory_bus)
      (Axi8)

  module Dma =
    System_dma_controller.Make (General_config) (Memory_controller.Memory_bus)
      (Memory_to_packet8)

  module Video_out_with_memory = Video_out.Make (Memory_controller.Memory_bus)

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
      { memory_clock : 'a Clocking.t
      ; video_clock : 'a Clocking.t
      ; hart_clock : 'a Clocking.t
      ; dma_clock : 'a Clocking.t
      ; uart_rx : 'a option [@exists include_uart_wires]
      ; memory : 'a Axi4.I.t [@rtlprefix "axi_i$"]
      }
    [@@deriving hardcaml ~rtlmangle:"$"]
  end

  module O = struct
    type 'a t =
      { registers : 'a Registers.t list [@length General_config.num_harts]
      ; uart_tx : 'a option [@exists include_uart_wires]
      ; uart_rx_valid : 'a option [@exists include_uart_wires]
      ; video_out : 'a Video_out_with_memory.O.t option [@exists include_video_out]
      ; memory : 'a Axi4.O.t [@rtlprefix "axi_o$"]
      }
    [@@deriving hardcaml ~rtlmangle:"$"]
  end

  let default_transaction (hart : _ Hart.O.t) =
    { With_valid.valid = vdd
    ; value =
        { Transaction.set_rd = vdd
        ; new_rd = zero register_width
        ; new_pc = hart.registers.pc +:. 4
        ; error = gnd
        }
    }
  ;;

  let assign_non_io_ecall ((hart : _ Hart.O.t), transaction) =
    (* Default the remainders *)
    Transaction.With_valid.Of_signal.(transaction <-- default_transaction hart)
  ;;

  let assign_empty_ecalls harts hart_ecall_transactions =
    List.zip_exn harts hart_ecall_transactions |> List.iter ~f:assign_non_io_ecall
  ;;

  let video_out_read_slot = 0
  let dma_read_slot = if include_video_out then 1 else 0
  let dma_write_slot = 0

  let assign_dma_io_ecall
        ~clock:_
        (harts : _ Hart.O.t list)
        hart_ecall_transactions
        ~tx_input
        ~tx_ready
        scope
    =
    (* For now we only allow Hart0 to do IO. This isn't
       necessary, but it makes it easier to stop them both
       issuing commands at the same time and entering a weird
       state. *)
    (* TODO: This is a terrible implementation of ecall, it would be better if there was a postbox style interface. *)
    let hart0 = List.nth_exn harts 0 in
    (* Delay the ecall by a cycle so we can register all the relevant
       registers, reducing routing pressure. *)
    (* TODO: This is pretty jank - move ecall to write back and pass 'ecall registers' out of the pipeline *)
    let%hw delayed_r5 = List.nth_exn hart0.registers.general 5 ==:. 0 in
    let%hw delayed_r6 = List.nth_exn hart0.registers.general 6 in
    let%hw delayed_r7 =
      List.nth_exn hart0.registers.general 7
      |> uresize ~width:Memory_to_packet8.Input.port_widths.length
    in
    let%hw is_dma_write = hart0.is_ecall &: delayed_r5 in
    let%hw next_pc = hart0.registers.pc +:. 4 in
    (* TODO: I think this can race. *)
    Memory_to_packet8.Input.With_valid.Of_signal.(
      tx_input
      <-- { valid = is_dma_write &: tx_ready
          ; value = { address = delayed_r6; length = delayed_r7 }
          });
    (* Assign the Hart0 transaction. *)
    Transaction.With_valid.Of_signal.(
      List.hd_exn hart_ecall_transactions
      <-- { With_valid.valid = vdd
          ; value =
              { Transaction.set_rd = vdd
              ; new_rd = uextend ~width:register_width (is_dma_write &: tx_ready)
              ; new_pc = next_pc
              ; error = gnd
              }
          });
    (* Default the remainders *)
    List.zip_exn harts hart_ecall_transactions
    |> List.tl_exn
    |> List.iter ~f:assign_non_io_ecall
  ;;

  let assign_ecalls ~clock ~tx_input_and_ready harts hart_ecall_transactions scope =
    (* If a DMA controller is in the design then wire up DMA related ecalls
       otherwise do not include any ecalls *)
    match tx_input_and_ready with
    | Some (tx_input, tx_ready) ->
      assign_dma_io_ecall ~clock ~tx_input ~tx_ready harts hart_ecall_transactions scope
    | None -> assign_empty_ecalls harts hart_ecall_transactions
  ;;

  module Video_data = struct
    type 'a t =
      { video_data : 'a Video_out_with_memory.O.t
      ; memory_request_ack : 'a Memory_controller.Memory_bus.Read_bus.Dest.t
      ; memory_request : 'a Memory_controller.Memory_bus.Read_bus.Source.t
      ; memory_response : 'a Memory_controller.Memory_bus.Read_response.With_valid.t
      }
    [@@deriving fields ~getters]
  end

  let maybe_video_out ~build_mode scope (i : _ I.t) =
    match General_config.include_video_out with
    | No_video_out -> None
    | Video_out
        ( (module Framebuffer_config : Video_out_intf.Config)
        , (module Video_signals_config : Video_signals.Config) ) ->
      let open Memory_controller.Cross_clocks in
      let read_request_ack_i =
        Memory_controller.Memory_bus.Read_bus.Dest.Of_signal.wires ()
      in
      let read_request_i =
        Memory_controller.Memory_bus.Read_bus.Source.Of_signal.wires ()
      in
      let read_response_i =
        Memory_controller.Memory_bus.Read_response.With_valid.Of_signal.wires ()
      in
      let read_request, read_request_ack =
        let o =
          maybe_cross_read_request
            ~build_mode
            ~clock_domain_memory:General_config.memory_domain
            ~clock_domain_user:Video_signals_config.clock_domain
            scope
            { Read.I.clocking_i = i.video_clock
            ; clocking_o = i.memory_clock
            ; i = read_request_i
            ; o = read_request_ack_i
            }
        in
        o.i, o.o
      in
      let read_response =
        (maybe_cross_read_response
           ~build_mode
           ~clock_domain_memory:General_config.memory_domain
           ~clock_domain_user:Video_signals_config.clock_domain
           scope
           { Read_response.I.clocking_i = i.memory_clock
           ; clocking_o = i.video_clock
           ; i = read_response_i
           })
          .i
      in
      let video_out =
        Video_out_with_memory.hierarchical
          ~framebuffer_config:(module Framebuffer_config)
          ~video_signals_config:(module Video_signals_config)
          scope
          { Video_out_with_memory.I.clock = i.video_clock
          ; memory_request = read_request_ack
          ; memory_response = read_response
          }
      in
      Memory_controller.Memory_bus.Read_bus.Source.Of_signal.(
        read_request_i <-- video_out.memory_request);
      Some
        { Video_data.video_data = video_out
        ; memory_request_ack = read_request_ack_i
        ; memory_request = read_request
        ; memory_response = read_response_i
        }
  ;;

  let initialize_harts
        ~build_mode
        ~io_clear
        ~hart_ecall_transactions
        ~read_instruction_bus_per_hart
        ~read_data_bus_per_hart
        ~write_bus_per_hart
        ~(memory_controller : _ Memory_controller.O.t)
        scope
        { I.hart_clock; memory_clock; _ }
    =
    let reg_spec_no_clear = Clocking.to_spec_no_clear hart_clock in
    let hart_clock =
      { hart_clock with
        clear = pipeline ~n:2 reg_spec_no_clear (io_clear |: hart_clock.clear)
      }
    in
    let get_instruction_channels which_hart =
      ( select_instruction_rd_chs_for_hart
          which_hart
          memory_controller.instruction.read_to_controller
      , select_instruction_rd_chs_for_hart
          which_hart
          memory_controller.instruction.read_response )
    in
    let get_data_channels which_hart =
      ( select_data_rd_chs_for_hart which_hart memory_controller.data.read_to_controller
      , select_data_wr_chs_for_hart which_hart memory_controller.data.write_to_controller
      , select_data_rd_chs_for_hart which_hart memory_controller.data.read_response
      , select_data_wr_chs_for_hart which_hart memory_controller.data.write_response )
    in
    let create_read_request_channel
          ~build_mode
          hart_clock
          memory_clock
          read_request_i
          read_request_ack
      =
      let open Memory_controller.Cross_clocks in
      let o =
        maybe_cross_read_request
          ~build_mode
          ~clock_domain_memory:General_config.memory_domain
          ~clock_domain_user:Hart_config.clock_domain
          scope
          { Read.I.clocking_i = hart_clock
          ; clocking_o = memory_clock
          ; i = read_request_i
          ; o = read_request_ack
          }
      in
      o.i, o.o
    in
    let create_write_request_channel
          ~build_mode
          hart_clock
          memory_clock
          write_request_i
          write_request_ack
      =
      let open Memory_controller.Cross_clocks in
      let o =
        maybe_cross_write_request
          ~build_mode
          ~clock_domain_memory:General_config.memory_domain
          ~clock_domain_user:Hart_config.clock_domain
          scope
          { Write.I.clocking_i = hart_clock
          ; clocking_o = memory_clock
          ; i = write_request_i
          ; o = write_request_ack
          }
      in
      o.i, o.o
    in
    let create_read_response_channel ~build_mode hart_clock memory_clock read_response =
      let open Memory_controller.Cross_clocks in
      maybe_cross_read_response
        ~build_mode
        ~clock_domain_memory:General_config.memory_domain
        ~clock_domain_user:Hart_config.clock_domain
        scope
        { Read_response.I.clocking_i = memory_clock
        ; clocking_o = hart_clock
        ; i = read_response
        }
    in
    let create_write_response_channel ~build_mode hart_clock memory_clock write_response =
      let open Memory_controller.Cross_clocks in
      maybe_cross_write_response
        ~build_mode
        ~clock_domain_memory:General_config.memory_domain
        ~clock_domain_user:Hart_config.clock_domain
        scope
        { Write_response.I.clocking_i = memory_clock
        ; clocking_o = hart_clock
        ; i = write_response
        }
    in
    (* This memory wrangling looks complicated but we are really just
       optionally async-fifo-ing six signals before passing them to the
       hart so that memory and the hart can use different clocks.

       This is a little fiddly since the requests have a handshake signal
       that we need to be consistent. As we do not handshake responses,
       that is unconditional. *)
    let harts =
      List.init
        ~f:(fun which_hart ->
          let instruction_read_request_ack, instruction_read_response =
            get_instruction_channels which_hart
          in
          let ( data_read_request_ack
              , data_write_request_ack
              , data_read_response
              , data_write_response )
            =
            get_data_channels which_hart
          in
          let instruction_read_request_i = Read_bus.Source.Of_signal.wires () in
          let data_read_request_i = Read_bus.Source.Of_signal.wires () in
          let data_write_request_i = Write_bus.Source.Of_signal.wires () in
          let instruction_read_request, instruction_read_request_ack =
            create_read_request_channel
              ~build_mode
              hart_clock
              memory_clock
              instruction_read_request_i
              instruction_read_request_ack
          in
          let data_read_request, data_read_request_ack =
            create_read_request_channel
              ~build_mode
              hart_clock
              memory_clock
              data_read_request_i
              data_read_request_ack
          in
          let data_write_request, data_write_request_ack =
            create_write_request_channel
              ~build_mode
              hart_clock
              memory_clock
              data_write_request_i
              data_write_request_ack
          in
          let instruction_read_response =
            create_read_response_channel
              ~build_mode
              hart_clock
              memory_clock
              instruction_read_response
          in
          let data_read_response =
            create_read_response_channel
              ~build_mode
              hart_clock
              memory_clock
              data_read_response
          in
          let data_write_response =
            create_write_response_channel
              ~build_mode
              hart_clock
              memory_clock
              data_write_response
          in
          let hart =
            Hart.hierarchical
              ~instance:[%string "hart_%{which_hart#Int}"]
              scope
              { Hart.I.clock = hart_clock
              ; read_instruction = instruction_read_request_ack
              ; read_data = data_read_request_ack
              ; write_data = data_write_request_ack
              ; read_instruction_response = instruction_read_response.i
              ; read_data_response = data_read_response.i
              ; write_data_response = data_write_response.i
              ; ecall_transaction = List.nth_exn hart_ecall_transactions which_hart
              }
          in
          Read_bus.Source.Of_signal.(
            instruction_read_request_i <-- Hart.O.read_instruction hart;
            data_read_request_i <-- Hart.O.read_data hart);
          Write_bus.Source.Of_signal.(data_write_request_i <-- Hart.O.write_data hart);
          { hart with
            read_instruction = instruction_read_request
          ; read_data = data_read_request
          ; write_data = data_write_request
          })
        General_config.num_harts
    in
    List.iteri
      ~f:(fun i h ->
        Read_bus.Source.Of_signal.(
          List.nth_exn (List.nth_exn read_instruction_bus_per_hart i) 0
          <-- Hart.O.read_instruction h;
          List.nth_exn (List.nth_exn read_data_bus_per_hart i) 0 <-- Hart.O.read_data h);
        Write_bus.Source.Of_signal.(
          List.nth_exn (List.nth_exn write_bus_per_hart i) 0 <-- Hart.O.write_data h))
      harts;
    harts
  ;;

  let create ~build_mode scope (i : _ I.t) =
    (* If the design has requested a video out then initialize it. *)
    let maybe_video_out = maybe_video_out ~build_mode scope i in
    (* If the design has requested a DMA controller then initialize it with a
       bunch of unconnected wires. These will be wired to the memory controller
       and harts below. *)
    let tx_input = Memory_to_packet8.Input.With_valid.Of_signal.wires () in
    let dma_read_request_i = Read_bus.Source.Of_signal.wires () in
    let dma_read_request_ack_i = Read_bus.Dest.Of_signal.wires () in
    let dma_write_request_i = Write_bus.Source.Of_signal.wires () in
    let dma_write_request_ack_i = Write_bus.Dest.Of_signal.wires () in
    let dma_read_response_i = Read_response.With_valid.Of_signal.wires () in
    let dma_write_response_i = Write_response.With_valid.Of_signal.wires () in
    let ( dma_read_request
        , dma_read_request_ack
        , dma_write_request
        , dma_write_request_ack
        , dma_read_response
        , dma_write_response )
      =
      let open Memory_controller.Cross_clocks in
      let read_request =
        maybe_cross_read_request
          ~build_mode
          ~clock_domain_memory:General_config.memory_domain
          ~clock_domain_user:General_config.dma_domain
          scope
          { Read.I.clocking_i = i.dma_clock
          ; clocking_o = i.memory_clock
          ; i = dma_read_request_i
          ; o = dma_read_request_ack_i
          }
      in
      let write_request =
        maybe_cross_write_request
          ~build_mode
          ~clock_domain_memory:General_config.memory_domain
          ~clock_domain_user:General_config.dma_domain
          scope
          { Write.I.clocking_i = i.dma_clock
          ; clocking_o = i.memory_clock
          ; i = dma_write_request_i
          ; o = dma_write_request_ack_i
          }
      in
      let read_response =
        maybe_cross_read_response
          ~build_mode
          ~clock_domain_memory:General_config.memory_domain
          ~clock_domain_user:General_config.dma_domain
          scope
          { Read_response.I.clocking_i = i.memory_clock
          ; clocking_o = i.dma_clock
          ; i = dma_read_response_i
          }
      in
      let write_response =
        maybe_cross_write_response
          ~build_mode
          ~clock_domain_memory:General_config.memory_domain
          ~clock_domain_user:General_config.dma_domain
          scope
          { Write_response.I.clocking_i = i.memory_clock
          ; clocking_o = i.dma_clock
          ; i = dma_write_response_i
          }
      in
      ( read_request.i
      , read_request.o
      , write_request.i
      , write_request.o
      , read_response.i
      , write_response.i )
    in
    let maybe_dma_controller =
      match General_config.include_io_controller with
      | No_io_controller -> None
      | Uart_controller config ->
        Some
          (Dma.hierarchical
             ~uart_config:config
             scope
             { Dma.I.clock = i.dma_clock
             ; uart_rx = Option.value_exn i.uart_rx
             ; tx_input
             ; read_request = dma_read_request_ack
             ; write_request = dma_write_request_ack
             ; read_response = dma_read_response
             ; write_response = dma_write_response
             })
    in
    let of_dma ~f = Option.map ~f maybe_dma_controller in
    let of_video_out ~f = Option.map ~f maybe_video_out in
    (* Initialize the memory controller and allocate some wires for channels.
       Any non-hart memory channels will occupy the start of the memory
       controller, followed by the memory channels of each hart. *)
    let read_instruction_bus_per_hart =
      List.init
        ~f:(fun _which_hart ->
          List.init ~f:(fun _i -> Read_bus.Source.Of_signal.wires ()) 1)
        General_config.num_harts
    in
    let read_data_bus_per_hart =
      List.init
        ~f:(fun _which_hart ->
          List.init ~f:(fun _i -> Read_bus.Source.Of_signal.wires ()) 1)
        General_config.num_harts
    in
    let write_bus_per_hart =
      List.init
        ~f:(fun _which_hart ->
          List.init ~f:(fun _i -> Write_bus.Source.Of_signal.wires ()) 1)
        General_config.num_harts
    in
    of_dma ~f:(fun dma -> [ dma.read_request ])
    |> Option.iter ~f:(fun t ->
      Read_bus.Source.Of_signal.(dma_read_request_i <-- List.nth_exn t 0));
    of_dma ~f:(fun dma -> [ dma.write_request ])
    |> Option.iter ~f:(fun t ->
      Write_bus.Source.Of_signal.(dma_write_request_i <-- List.nth_exn t 0));
    let controller =
      Memory_controller.hierarchical
        ~build_mode
        ~priority_mode:Priority_order
        scope
        { Memory_controller.I.clock = i.memory_clock
        ; instruction =
            { read_to_controller = List.concat read_instruction_bus_per_hart
            ; write_to_controller = []
            }
        ; data =
            { read_to_controller =
                (of_video_out ~f:(fun vd -> [ vd.memory_request ])
                 |> Option.value ~default:[])
                @ (of_dma ~f:(fun _dma -> [ dma_read_request ])
                   |> Option.value ~default:[])
                @ List.concat read_data_bus_per_hart
            ; write_to_controller =
                (of_dma ~f:(fun _dma -> [ dma_write_request ]) |> Option.value ~default:[])
                @ List.concat write_bus_per_hart
            }
        ; memory = i.memory
        }
    in
    let hart_ecall_transactions =
      List.init
        ~f:(fun _ -> Transaction.With_valid.Of_signal.wires ())
        General_config.num_harts
    in
    let harts =
      initialize_harts
        ~build_mode
        ~io_clear:(of_dma ~f:Dma.O.clear_message |> Option.value ~default:gnd)
          (* Clear harts when the DMA core pulses clear in addition to the
             global clear if a DMA controller is attached. *)
        ~hart_ecall_transactions
        ~write_bus_per_hart
        ~read_instruction_bus_per_hart
        ~read_data_bus_per_hart
        ~memory_controller:controller
        scope
        i
    in
    assign_ecalls (* TODO: If DMA clock <> hart clock this is not compatible. *)
      ~clock:i.hart_clock
      ~tx_input_and_ready:
        (match maybe_dma_controller with
         | Some dma -> Some (tx_input, dma.dma_tx_ready)
         | None -> None)
      harts
      hart_ecall_transactions
      scope;
    Option.iter
      ~f:(fun video_out ->
        Read_bus.Dest.Of_signal.(
          video_out.memory_request_ack
          <-- List.nth_exn controller.data.read_to_controller video_out_read_slot);
        Read_response.With_valid.Of_signal.(
          video_out.memory_response
          <-- List.nth_exn controller.data.read_response video_out_read_slot))
      maybe_video_out;
    (match maybe_dma_controller with
     | Some _ ->
       Read_bus.Dest.Of_signal.(
         dma_read_request_ack_i
         <-- List.nth_exn controller.data.read_to_controller dma_read_slot);
       Read_response.With_valid.Of_signal.(
         dma_read_response_i <-- List.nth_exn controller.data.read_response dma_read_slot);
       Write_bus.Dest.Of_signal.(
         dma_write_request_ack_i
         <-- List.nth_exn controller.data.write_to_controller dma_write_slot);
       Write_response.With_valid.Of_signal.(
         dma_write_response_i
         <-- List.nth_exn controller.data.write_response dma_write_slot)
     | None -> ());
    { O.registers = List.map ~f:(fun o -> o.registers) harts
    ; uart_tx = of_dma ~f:Dma.O.uart_tx
    ; uart_rx_valid = of_dma ~f:Dma.O.uart_rx_valid
    ; video_out = of_video_out ~f:Video_data.video_data
    ; memory = controller.memory
    }
  ;;

  let hierarchical ~build_mode (scope : Scope.t) (input : Signal.t I.t) =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ~scope ~name:"system" (create ~build_mode) input
  ;;
end
