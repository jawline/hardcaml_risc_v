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

  let create (scope : Scope.t) ({ I.clock; clear; _ } : _ I.t) =
    let ( -- ) = Scope.naming scope in
    let reg_spec = Reg_spec.create ~clock ~clear () in
    let reg_spec_no_clear = Reg_spec.create ~clock () in
    let state = State_machine.create (module State) reg_spec in
    let done_ = Variable.wire ~default:gnd in
    ignore (state.current -- "current_state" : Signal.t);
    { O.busy = ~:(state.is State.Idle)
    ; done_ = done_.value
    ; output_packet = assert false
    ; memory = assert false
    ; memory_response = assert false
    }
  ;;

  let hierarchical ~instance (scope : Scope.t) (input : Signal.t I.t) =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ~scope ~name:"dma_memory_to_packet" ~instance create input
  ;;
end
