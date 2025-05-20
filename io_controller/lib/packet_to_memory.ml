open! Core
open Hardcaml
open Hardcaml_axi
open Hardcaml_io_framework
open Hardcaml_memory_controller
open Signal
open Always

module Make (Memory : Memory_bus_intf.S) (Axi : Stream.S) = struct
  module Word_buffer = Data_resize.Make (struct
      let input_width = 8
      let output_width = 32
    end)

  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; in_ : 'a Axi.Source.t
      ; out : 'a Memory.Write_bus.Rx.t
      ; out_ack : 'a Memory.Write_response.With_valid.t
      }
    [@@deriving hardcaml ~rtlmangle:"$"]
  end

  module O = struct
    type 'a t =
      { in_ : 'a Axi.Dest.t
      ; out : 'a Memory.Write_bus.Tx.t
      }
    [@@deriving hardcaml ~rtlmangle:"$"]
  end

  module State = struct
    type t =
      | Reading_memory_address
      | Transferring
      | Transfer_final_beat
      | Ignoring_illegal_address
    [@@deriving sexp, enumerate, compare]
  end

  let create (scope : Scope.t) ({ I.clock; clear; in_; out; out_ack = _ } : _ I.t) =
    let ( -- ) = Scope.naming scope in
    let reg_spec = Reg_spec.create ~clock ~clear () in
    let state = State_machine.create (module State) reg_spec in
    ignore (state.current -- "current_state" : Signal.t);
    if Memory.data_bus_width % Axi.Source.port_widths.tdata <> 0
    then raise_s [%message "BUG: Memory width must be a multiple of DMA stream input"];
    let current_address =
      Variable.reg ~width:Word_buffer.O.port_widths.out_data reg_spec
    in
    let clear_buffers = Always.Variable.wire ~default:gnd in
    let word_buffer =
      Word_buffer.hierarchical
        scope
        { Word_buffer.I.clock
        ; clear = clear |: clear_buffers.value
        ; in_valid =
            in_.tvalid &: (state.is Transferring |: state.is Reading_memory_address)
        ; in_data = in_.tdata
        ; out_ready =
            state.is Reading_memory_address |: (state.is Transferring &: out.ready)
        }
    in
    let reset = proc [ clear_buffers <-- vdd; state.set_next Reading_memory_address ] in
    compile
      [ state.switch
          [ ( State.Reading_memory_address
            , [ when_
                  word_buffer.out_valid
                  [ current_address <-- word_buffer.out_data
                  ; if_
                      (Memory.address_is_word_aligned word_buffer.out_data)
                      [ state.set_next Transferring ]
                      [ state.set_next Ignoring_illegal_address ]
                  ]
              ; when_ (in_.tvalid &: in_.tlast) [ reset ]
              ] )
          ; ( Transferring
            , [ when_ (in_.tvalid &: in_.tlast) [ state.set_next Transfer_final_beat ]
              ; when_ (word_buffer.out_valid &: out.ready) [ incr ~by:4 current_address ]
              ] )
          ; ( Transfer_final_beat
            , [ when_ (out.ready |: ~:(word_buffer.interim_data_buffered)) [ reset ] ] )
          ; Ignoring_illegal_address, [ when_ (in_.tvalid &: in_.tlast) [ reset ] ]
          ]
      ];
    { O.in_ = { tready = word_buffer.ready |: state.is Ignoring_illegal_address }
    ; out =
        { valid =
            state.is Transferring
            &: word_buffer.out_valid
            |: (state.is Transfer_final_beat &: word_buffer.interim_data_buffered)
        ; data =
            { address = current_address.value; write_data = bswap word_buffer.out_data }
        }
    }
  ;;

  let hierarchical ~instance (scope : Scope.t) (input : Signal.t I.t) =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ~scope ~name:"packet_to_memory" ~instance create input
  ;;
end
