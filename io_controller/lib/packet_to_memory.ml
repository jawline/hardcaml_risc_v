open! Core
open Hardcaml
open Hardcaml_axi
open Hardcaml_io_framework
open Hardcaml_memory_controller
open Signal
open Always

module Make (Memory : Memory_bus_intf.S) (Axi : Stream.S) = struct
  let address_width = Memory.address_width
  let data_width = Memory.data_bus_width

  module Address_buffer = Data_resize.Make (struct
      let input_width = 8
      let output_width = 32
    end)

  module Word_buffer = Data_resize.Make (struct
      let input_width = 8
      let output_width = data_width
    end)

  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; in_ : 'a Axi.Source.t
      ; out : 'a Memory.Write_bus.Dest.t
      ; out_ack : 'a Memory.Write_response.With_valid.t
      }
    [@@deriving hardcaml ~rtlmangle:"$"]
  end

  module O = struct
    type 'a t =
      { in_ : 'a Axi.Dest.t
      ; out : 'a Memory.Write_bus.Source.t
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
    let reg_spec = Reg_spec.create ~clock ~clear () in
    let%hw.State_machine state = State_machine.create (module State) reg_spec in
    if Memory.data_bus_width % Axi.Source.port_widths.tdata <> 0
    then raise_s [%message "BUG: Memory width must be a multiple of DMA stream input"];
    let current_address = Variable.reg ~width:address_width reg_spec in
    let reset_buffers = Variable.wire ~default:gnd in
    let address_buffer =
      Address_buffer.hierarchical
        scope
        { Address_buffer.I.clock
        ; clear = clear |: reset_buffers.value
        ; in_valid = in_.tvalid &: state.is Reading_memory_address
        ; in_data = in_.tdata
        ; out_ready = vdd
        }
    in
    let word_buffer =
      Word_buffer.hierarchical
        scope
        { Word_buffer.I.clock
        ; clear = clear |: reset_buffers.value
        ; in_valid = in_.tvalid &: state.is Transferring
        ; in_data = in_.tdata
        ; out_ready = out.ready
        }
    in
    let reset = proc [ state.set_next Reading_memory_address; reset_buffers <-- vdd ] in
    let address_aligned = Memory.byte_address_to_memory_address address_buffer.out_data in
    compile
      [ state.switch
          [ ( State.Reading_memory_address
            , [ when_
                  address_buffer.out_valid
                  [ current_address <-- address_aligned.value
                  ; if_
                      address_aligned.valid
                      [ state.set_next Transferring ]
                      [ state.set_next Ignoring_illegal_address ]
                  ]
              ; when_ (in_.tvalid &: in_.tlast) [ reset ]
              ] )
          ; ( Transferring
            , [ when_ (in_.tvalid &: in_.tlast) [ state.set_next Transfer_final_beat ]
              ; when_ (word_buffer.out_valid &: out.ready) [ incr current_address ]
              ] )
          ; ( Transfer_final_beat
            , [ when_ (out.ready |: ~:(word_buffer.interim_data_buffered)) [ reset ] ] )
          ; Ignoring_illegal_address, [ when_ (in_.tvalid &: in_.tlast) [ reset ] ]
          ]
      ];
    { O.in_ =
        { tready =
            state.is Reading_memory_address
            &: address_buffer.ready
            |: (state.is Transferring &: word_buffer.ready)
            |: state.is Ignoring_illegal_address
        }
    ; out =
        { valid =
            state.is Transferring
            &: word_buffer.out_valid
            |: (state.is Transfer_final_beat &: word_buffer.interim_data_buffered)
        ; data =
            { address = current_address.value
            ; write_data = bswap word_buffer.out_data
            ; wstrb = ones (width word_buffer.out_data / 8) (* TODO: Byte enables *)
            }
        }
    }
  ;;

  let hierarchical (scope : Scope.t) (input : Signal.t I.t) =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ~scope ~name:"packet_to_memory" create input
  ;;
end
