open! Core
open Hardcaml
open Hardcaml_memory_controller
open Signal
open Always

module Make (Memory : Memory_bus_intf.S) (P : Packet_intf.S) = struct
  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; in_ : 'a P.Contents_stream.Tx.t
      ; out : 'a Memory.Tx_bus.Rx.t
      ; out_ack : 'a Memory.Rx_bus.Tx.t
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t =
      { in_ : 'a P.Contents_stream.Rx.t
      ; out : 'a Memory.Tx_bus.Tx.t
      ; out_ack : 'a Memory.Rx_bus.Rx.t
      }
    [@@deriving sexp_of, hardcaml]
  end

  module State = struct
    type t =
      | Reading_memory_address
      | Buffering_word
      | Writing_word
    [@@deriving sexp, enumerate, compare]
  end

  let create (scope : Scope.t) ({ I.clock; clear; in_; out; out_ack } : _ I.t) =
    let ( -- ) = Scope.naming scope in
    let reg_spec = Reg_spec.create ~clock ~clear () in
    let reg_spec_no_clear = Reg_spec.create ~clock () in
    let state = State_machine.create (module State) reg_spec in
    ignore (state.current -- "current_state" : Signal.t);
    if Memory.data_bus_width % width in_.data.data <> 0
    then raise_s [%message "BUG: Memory width must be a multiple of DMA stream input"];
    let num_cycle_to_buffer_entire_memory_address =
      Memory.data_bus_width / width in_.data.data
    in
    let input_ready = Variable.wire ~default:(zero 1) in
    compile
      [ state.switch
          [ State.Reading_memory_address, []
          ; State.Buffering_word, []
          ; State.Writing_word, []
          ]
      ];
    assert false
  ;;

  let hierarchical ~instance (scope : Scope.t) (input : Signal.t I.t) =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ~scope ~name:"DMA" ~instance create input
  ;;
end
