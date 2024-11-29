(* Write a packet from memory. The framing format is a 2 byte length tag
   followed by data.

   The module takes an enable signal with a length and address and writes
   out the length (Little-endian) and then the memory byte by byte to
   an output stream. When connected to a Uart_tx this should allow us
   to communicate with the host.

   Currently this module does not prefetch memory while writing, which would
   be a straightforward improvement.
*)
open! Core
open Hardcaml
open Hardcaml_memory_controller
open Signal
open Always

module Packet8 = Packet.Make (struct
    let data_bus_width = 8
  end)

module Make (Config : Memory_to_packet8_intf.Config) (Memory : Memory_bus_intf.S) = struct
  module Input = struct
    module T = struct
      type 'a t =
        { length : 'a [@bits 16]
        ; address : 'a [@bits Memory.data_bus_width]
        }
      [@@deriving hardcaml]
    end

    include T
    module With_valid = With_valid.Wrap.Make (T)
  end

  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; enable : 'a Input.With_valid.t
      ; output_packet : 'a Packet8.Contents_stream.Rx.t
      ; memory : 'a Memory.Read_bus.Rx.t
      ; memory_response : 'a Memory.Read_response.With_valid.t
      }
    [@@deriving hardcaml ~rtlmangle:"$"]
  end

  module O = struct
    type 'a t =
      { busy : 'a
      ; done_ : 'a
      ; output_packet : 'a Packet8.Contents_stream.Tx.t [@rtlprefix "output$"]
      ; memory : 'a Memory.Read_bus.Tx.t [@rtlprefix "memory$"]
      }
    [@@deriving hardcaml ~rtlmangle:"$"]
  end

  module State = struct
    type t =
      | Idle
      | Writing_header
      | Writing_length
      | Reading_data
      | Writing_data
    [@@deriving sexp, enumerate, compare]
  end

  let create
    (scope : Scope.t)
    ({ I.clock
     ; clear
     ; enable =
         { valid = input_enable
         ; value = { length = input_length; address = input_address }
         }
     ; memory_response
     ; output_packet = { ready = output_packet_ready }
     ; memory = memory_ack
     } :
      _ I.t)
    =
    let ( -- ) = Scope.naming scope in
    let reg_spec = Reg_spec.create ~clock ~clear () in
    let reg_spec_no_clear = Reg_spec.create ~clock () in
    let state = State_machine.create (module State) reg_spec in
    let done_ = Variable.wire ~default:gnd in
    let length = Variable.reg ~width:(width input_length) reg_spec_no_clear in
    let address = Variable.reg ~width:(width input_address) reg_spec_no_clear in
    let which_step = Variable.reg ~width:2 reg_spec_no_clear in
    ignore (state.current -- "current_state" : Signal.t);
    let output_packet = Packet8.Contents_stream.Tx.Of_always.wire zero in
    let address_stride = width memory_response.value.read_data / 8 in
    let read_data =
      Variable.reg ~width:(width memory_response.value.read_data) reg_spec_no_clear
    in
    let alignment_mask =
      width memory_response.value.read_data / 8 |> Int.floor_log2 |> ones
    in
    let do_read = Variable.reg ~width:1 reg_spec_no_clear in
    let enter_reading_data = proc [ state.set_next Reading_data; do_read <-- vdd ] in
    compile
      [ state.switch
          [ ( State.Idle
            , [ (* Guard against entering the state machine
                   when zero length is requested to avoid sending null
                   packets out. This isn't strictly necessary but makes
                   the state machine much easier to think about. *)
                when_
                  (input_enable &: (input_length <>:. 0))
                  [ length <-- input_length
                  ; address <-- input_address
                  ; which_step <--. 0
                  ; (match Config.header with
                     | Some _ -> Writing_header
                     | None -> Writing_length)
                    |> state.set_next
                  ; do_read <-- gnd
                  ]
              ] )
          ; ( Writing_header
            , match Config.header with
              | Some header ->
                [ Packet8.Contents_stream.Tx.Of_always.assign
                    output_packet
                    { valid = vdd; data = { data = Signal.of_char header; last = gnd } }
                ; when_ output_packet_ready [ state.set_next Writing_length ]
                ]
              | None -> [] )
          ; ( Writing_length
            , let length_byte =
                mux
                  (which_step.value -- "which_step")
                  (split_msb ~part_width:8 length.value)
              in
              [ Packet8.Contents_stream.Tx.Of_always.assign
                  output_packet
                  { valid = vdd; data = { data = length_byte; last = gnd } }
              ; when_
                  output_packet_ready
                  [ incr which_step
                  ; when_
                      (which_step.value ==:. 1)
                      [ (* If the address was unaligned, set which_step to the
                           offset here to align it. *)
                        which_step
                        <-- (uresize ~width:(width alignment_mask) address.value
                             &: alignment_mask)
                      ; (* Align the address we read. Which step will
                           make sure we do not write the lower bytes. *)
                        address
                        <-- (address.value
                             &: ~:(uresize ~width:(width address.value) alignment_mask))
                      ; enter_reading_data
                      ]
                  ]
              ] )
          ; ( Reading_data
            , [ (* We will lower the memory request when the memory controller acks then wait for the response. *)
                when_ memory_ack.ready [ do_read <-- gnd ]
              ; (* There will only be one request in flight on our line so we don't need to worry about other data. *)
                when_
                  (memory_response.valid -- "is_memory_response_valid")
                  [ (* Memory read can fail, if they do return zero. *)
                    read_data
                    <-- mux2
                          memory_response.value.error
                          (zero (width memory_response.value.read_data))
                          memory_response.value.read_data
                  ; state.set_next Writing_data
                  ]
              ] )
          ; ( Writing_data
            , [ Packet8.Contents_stream.Tx.Of_always.assign
                  output_packet
                  { valid = vdd
                  ; data =
                      { data =
                          mux which_step.value (split_lsb ~part_width:8 read_data.value)
                      ; last = length.value ==:. 1
                      }
                  }
              ; when_
                  output_packet_ready
                  [ decr length
                  ; incr which_step
                  ; (* TODO: Once we have exhausted our read, we return
                       to reading data. We could prefetch here to speed this
                       up and avoid the stall. *)
                    when_
                      (which_step.value ==:. address_stride - 1)
                      [ which_step <--. 0
                      ; incr ~by:address_stride address
                      ; enter_reading_data
                      ]
                  ; (* If this was the last write, reset the entire state machine to idle. *)
                    when_
                      (length.value ==:. 1)
                      [ done_ <-- vdd; which_step <--. 0; state.set_next Idle ]
                  ]
              ] )
          ]
      ];
    { O.busy = ~:(state.is State.Idle)
    ; done_ = done_.value
    ; output_packet = Packet8.Contents_stream.Tx.Of_always.value output_packet
    ; memory = { valid = do_read.value; data = { address = address.value } }
    }
  ;;

  let hierarchical ~instance (scope : Scope.t) (input : Signal.t I.t) =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ~scope ~name:"dma_memory_to_packet" ~instance create input
  ;;
end
