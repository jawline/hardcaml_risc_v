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
    [@@deriving sexp_of, hardcaml ~rtlmangle:"$"]
  end

  module O = struct
    type 'a t =
      { in_ : 'a P.Contents_stream.Rx.t
      ; out : 'a Memory.Tx_bus.Tx.t
      ; out_ack : 'a Memory.Rx_bus.Rx.t
      }
    [@@deriving sexp_of, hardcaml ~rtlmangle:"$"]
  end

  module State = struct
    type t =
      | Reading_memory_address
      | Buffering_word
      | Consume_remaining_buffer
      | Writing_word
      | Ignoring_illegal_address
    [@@deriving sexp, enumerate, compare]
  end

  let buffer_n_elements
    ?(reset_when = gnd)
    ~reg_spec
    ~reg_spec_no_clear
    ~n
    (input : Signal.t With_valid.t)
    =
    let next_element =
      reg_fb
        ~width:(Int.ceil_log2 n)
        ~f:(fun t ->
          mux2
            reset_when
            (of_int ~width:(width t) 0)
            (mux2 input.valid (mod_counter ~max:(n - 1) t) t))
        reg_spec
    in
    let data_parts =
      List.init
        ~f:(fun i ->
          reg_fb
            ~width:(width input.value)
            ~f:(fun t ->
              (* We wipe the state of the buffer on the first cycle
                 of a buffering round to make sure previous state doesn't leak into
                 the next DMA request and instead its zeroed out. *)
              mux2
                (input.valid &: (next_element ==:. i))
                input.value
                (mux2 (next_element ==:. 0) (zero (width t)) t))
            reg_spec_no_clear)
        n
    in
    { With_valid.valid =
        next_element ==:. n - 1 &: input.valid &: ~:reset_when |> reg reg_spec_no_clear
    ; value = data_parts
    }
  ;;

  let create (scope : Scope.t) ({ I.clock; clear; in_; out; out_ack = _ } : _ I.t) =
    let ( -- ) = Scope.naming scope in
    let reg_spec = Reg_spec.create ~clock ~clear () in
    let reg_spec_no_clear = Reg_spec.create ~clock () in
    let state = State_machine.create (module State) reg_spec in
    ignore (state.current -- "current_state" : Signal.t);
    if Memory.data_bus_width % width in_.data.data <> 0
    then raise_s [%message "BUG: Memory width must be a multiple of DMA stream input"];
    (* We assume that memory address width is data width here *)
    let num_cycle_to_buffer = Memory.data_bus_width / width in_.data.data in
    let input_ready = Variable.wire ~default:gnd in
    let address_buffer =
      buffer_n_elements
      (* Reset when last so we don't get stuck with a mis-sized packet. *)
        ~reset_when:in_.data.last
        ~reg_spec
        ~reg_spec_no_clear
        ~n:num_cycle_to_buffer
        { valid = state.is Reading_memory_address &: in_.valid; value = in_.data.data }
      |> With_valid.map_value ~f:concat_msb
    in
    let data_buffer =
      buffer_n_elements
        ~reg_spec
        ~reg_spec_no_clear
        ~n:num_cycle_to_buffer
        { valid = state.is Buffering_word &: in_.valid; value = in_.data.data }
      |> With_valid.map_value ~f:concat_lsb
    in
    (* TODO: We copy the memory address to current_address so we can increment
       it as we write. It's a little bit wasteful to have two registers where
       one won't be used at once - consider refactoring down the line. *)
    let current_address =
      Variable.reg ~width:(width address_buffer.value) reg_spec_no_clear
    in
    let current_data = Variable.reg ~width:(width data_buffer.value) reg_spec_no_clear in
    let was_last = Variable.reg ~width:1 reg_spec_no_clear in
    compile
      [ state.switch
          [ ( State.Reading_memory_address
            , [ input_ready <--. 1
              ; when_
                  address_buffer.valid
                  [ current_address <-- address_buffer.value -- "address_buffer"
                  ; if_
                      (Memory.address_is_word_aligned address_buffer.value)
                      [ state.set_next Buffering_word ]
                      [ state.set_next Ignoring_illegal_address ]
                  ]
              ] )
          ; (* TODO: In principle, we can buffer a word while waiting for a
               write, but that makes the state machine more complicated. *)
            ( Buffering_word
            , [ input_ready <--. 1
              ; (* If in_.last we will reset to Reading_memory_address unless
                   next_data.valid is also high in which case we will write the last
                   word first. This causes us to loose the last bytes of an unaligned
                   write. *)
                was_last <-- in_.data.last
              ; current_data <-- data_buffer.value -- "data_buffer"
              ; when_ in_.data.last [ state.set_next Consume_remaining_buffer ]
              ; when_ data_buffer.valid [ state.set_next Writing_word ]
              ] )
          ; ( Consume_remaining_buffer
            , [ (* We pause a cycle to let the in_.data.data get into the data buffer *)
                current_data <-- data_buffer.value -- "data_buffer"
              ; state.set_next Writing_word
              ] )
          ; ( Writing_word
            , [ when_
                  out.ready
                  [ current_address
                    <-- current_address.value +:. (width data_buffer.value / 8)
                  ; state.set_next Buffering_word
                  ]
              ] )
          ; ( Ignoring_illegal_address
            , [ (* Ignore the packet *)
                input_ready <--. 1
              ; when_ in_.data.last [ state.set_next Reading_memory_address ]
              ] )
          ]
      ];
    { O.in_ = { ready = input_ready.value }
    ; out =
        { valid = state.is Writing_word -- "data_out_valid"
        ; data =
            { address = current_address.value -- "data_out_address"
            ; write = state.is Writing_word
            ; write_data = current_data.value -- "data_out"
            }
        }
    ; out_ack = { ready = vdd }
    }
  ;;

  let hierarchical ~instance (scope : Scope.t) (input : Signal.t I.t) =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ~scope ~name:"dma" ~instance create input
  ;;
end
