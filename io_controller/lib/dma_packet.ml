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

  let buffer_n_elements ~reg_spec ~reg_spec_no_clear ~n (input : Signal.t With_valid.t) =
    let next_element =
      reg_fb
        ~width:(Int.ceil_log2 n)
        ~f:(fun t -> mux2 input.valid (mod_counter ~max:(n - 1) t) t)
        reg_spec
    in
    let data_parts =
      List.init
        ~f:(fun i ->
          reg_fb
            ~width:(width input.value)
            ~f:(fun t -> mux2 (next_element ==:. i) input.value t)
            reg_spec_no_clear)
        n
    in
    { With_valid.valid =
        (* We are using a valid signal to indicate the state machine should
           move forward, so we don't register it. The state
           of the value will not be correct until the cycle after a pulse. *)
        next_element ==:. n - 1 &: input.valid
    ; value = concat_msb data_parts
    }
  ;;

  let create (scope : Scope.t) ({ I.clock; clear; in_; out; out_ack } : _ I.t) =
    let ( -- ) = Scope.naming scope in
    let reg_spec = Reg_spec.create ~clock ~clear () in
    let reg_spec_no_clear = Reg_spec.create ~clock () in
    let state = State_machine.create (module State) reg_spec in
    ignore (state.current -- "current_state" : Signal.t);
    if Memory.data_bus_width % width in_.data.data <> 0
    then raise_s [%message "BUG: Memory width must be a multiple of DMA stream input"];
    (* We assume that memory address width is data width here *)
    let num_cycle_to_buffer = Memory.data_bus_width / width in_.data.data in
    let input_ready = Variable.wire ~default:(zero 1) in
    let address =
      buffer_n_elements
        ~reg_spec
        ~reg_spec_no_clear
        ~n:num_cycle_to_buffer
        { valid = state.is Reading_memory_address &: in_.valid; value = in_.data.data }
    in
    let next_data =
      buffer_n_elements
        ~reg_spec
        ~reg_spec_no_clear
        ~n:num_cycle_to_buffer
        { valid = state.is Buffering_word &: in_.valid; value = in_.data.data }
    in
    compile
      [ state.switch
          [ ( State.Reading_memory_address
            , [ input_ready <--. 1
                (* HERE: Discard on last so we don't get stuck. We need to reset address. *)
              ; when_ address.valid [ state.set_next Buffering_word ]
              ] )
          ; ( State.Buffering_word
            , [ input_ready <--. 1 (* HERE: Discard on last so we don't get stuck. *)
              ; when_ next_data.valid [ state.set_next Writing_word ]
              ] )
          ; ( State.Writing_word
            , [ (* HERE: Write word to memory address and then increment memory address before returning to buffering word. *)
                assert false
              ] )
          ]
      ];
    assert false
  ;;

  let hierarchical ~instance (scope : Scope.t) (input : Signal.t I.t) =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ~scope ~name:"DMA" ~instance create input
  ;;
end
