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
      [@@deriving sexp_of, hardcaml]
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
      ; memory : 'a Memory.Tx_bus.Rx.t
      ; memory_response : 'a Memory.Rx_bus.Tx.t
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t =
      { busy : 'a
      ; done_ : 'a
      ; output_packet : 'a Packet8.Contents_stream.Tx.t
      ; memory : 'a Memory.Tx_bus.Tx.t
      ; memory_response : 'a Memory.Rx_bus.Rx.t
      }
    [@@deriving sexp_of, hardcaml]
  end

  module State = struct
    type t =
      | Idle
      | (* TODO: Reword magic to header byte in all instances. *) Writing_magic
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
     ; (* We don't really care about memory acks. *) memory = _
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
    let which_step = Variable.reg ~width:4 reg_spec_no_clear in
    ignore (state.current -- "current_state" : Signal.t);
    let output_packet = Packet8.Contents_stream.Tx.Of_always.wire zero in
    let address_stride = width memory_response.data.read_data / 8 in
    let memory = Memory.Tx_bus.Tx.Of_always.wire zero in
    let read_data =
      Variable.reg ~width:(width memory_response.data.read_data) reg_spec_no_clear
    in
    (* TODO: Address alignment or support unaligned addresses? *)
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
                  ; (match Config.magic with
                     | Some _ -> Writing_magic
                     | None -> Writing_length)
                    |> state.set_next
                  ]
              ] )
          ; ( Writing_magic
            , match Config.magic with
              | Some magic ->
                [ Packet8.Contents_stream.Tx.Of_always.assign
                    output_packet
                    { valid = vdd; data = { data = Signal.of_char magic; last = gnd } }
                ; when_ output_packet_ready [ state.set_next Writing_length ]
                ]
              | None -> [] )
          ; ( Writing_length
            , let length_byte =
                mux which_step.value (split_msb ~part_width:8 length.value)
              in
              [ Packet8.Contents_stream.Tx.Of_always.assign
                  output_packet
                  { valid = vdd; data = { data = length_byte; last = gnd } }
              ; which_step <-- which_step.value +:. 1
              ; when_
                  (which_step.value ==:. 1)
                  [ which_step <--. 0
                  ; state.set_next Reading_data
                  ; (* If the address was unaligned, set which_step to the offset here to align it. *)
                    assert false
                  ]
              ] )
          ; ( Reading_data
            , [ Memory.Tx_bus.Tx.Of_always.assign
                  memory
                  { valid = vdd
                  ; data =
                      { address = address.value
                      ; write = gnd
                      ; write_data = zero (width memory_response.data.read_data)
                      }
                  }
              ; when_
                  memory_response.valid
                  [ (* Memory read can fail, if they do return zero. *)
                    read_data
                    <-- mux2
                          memory_response.data.error
                          (zero (width memory_response.data.read_data))
                          memory_response.data.read_data
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
                  [ (* We decrement length and increment address so that
                       the read address and stop conditions are updated. *)
                    address <-- address.value +:. 1
                  ; length <-- length.value -:. 1
                  ; which_step <-- which_step.value +:. 1
                  ; (* TODO: Once we have exhausted our read, we return
                       to reading data. We could prefetch here to speed this
                       up and avoid the stall. *)
                    when_
                      (which_step.value ==:. address_stride - 1)
                      [ which_step <--. 0; state.set_next Reading_data ]
                  ; (* If this was the last write, reset the entire state machine to idle. *)
                    when_ (length.value ==:. 1) [ which_step <--. 0; state.set_next Idle ]
                  ]
              ] )
          ]
      ];
    { O.busy = ~:(state.is State.Idle)
    ; done_ = done_.value
    ; output_packet = Packet8.Contents_stream.Tx.Of_always.value output_packet
    ; memory = Memory.Tx_bus.Tx.Of_always.value memory
    ; memory_response =
        { ready =
            (* We should always be ready to ack a read on the same cycle it becomes ready. *)
            vdd
        }
    }
  ;;

  let hierarchical ~instance (scope : Scope.t) (input : Signal.t I.t) =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ~scope ~name:"dma_memory_to_packet" ~instance create input
  ;;
end
