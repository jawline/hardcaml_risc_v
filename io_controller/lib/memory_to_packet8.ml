(* Write a packet from memory. The framing format is a 2 byte length tag
   followed by data. *)
open! Core
open Hardcaml
open Hardcaml_memory_controller
open Signal
open Always

module Packet8 = Packet.Make (struct
    let data_bus_width = 8
  end)

module Make (Memory : Memory_bus_intf.S) = struct
  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; enable : 'a
      ; length : 'a [@bits 16]
      ; address : 'a [@bits Memory.data_bus_width]
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
      | Writing_length
      | Reading_data
      | Writing_data
    [@@deriving sexp, enumerate, compare]
  end

  let create
    (scope : Scope.t)
    ({ I.clock
     ; clear
     ; enable
     ; length = input_length
     ; address = input_address
     ; memory_response
     ; _
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
    compile
      [ state.switch
          [ ( State.Idle
            , [ (* Guard against entering the state machine
                   when zero length is requested to avoid sending null
                   packets out. This isn't strictly necessary but makes
                   the state machine much easier to think about. *)
                when_
                  (enable &: (input_length <>:. 0))
                  [ length <-- input_length
                  ; address <-- input_address
                  ; which_step <--. 0
                  ; state.set_next Writing_length
                  ]
              ] )
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
                  [ which_step <--. 0; state.set_next Reading_data ]
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
          ; Writing_data, []
          ]
      ];
    { O.busy = ~:(state.is State.Idle)
    ; done_ = done_.value
    ; output_packet = Packet8.Contents_stream.Tx.Of_always.value output_packet
    ; memory = assert false
    ; memory_response = assert false
    }
  ;;

  let hierarchical ~instance (scope : Scope.t) (input : Signal.t I.t) =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ~scope ~name:"dma_memory_to_packet" ~instance create input
  ;;
end
