open! Core
open Hardcaml
open Signal
open! Always

(* This expects packets from a serial port in the form [ magic ; length MSB ;
   length LSB ; data ... ]. If a second packet is received before this packet
   is flushed out then it will be dropped so the serial sender is responsible
   for waiting for acknowledgement. *)

(* TODO: Consider adding a checksum. *)

module Make
    (Config : sig
       val magic : char
       val serial_input_width : int
       val max_packet_length_in_data_widths : int
     end)
    (P : Packet_intf.S) =
struct
  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; in_valid : 'a
      ; in_data : 'a
      ; out : 'a P.Contents_stream.Rx.t
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t = { out : 'a P.Contents_stream.Tx.t } [@@deriving sexp_of, hardcaml]
  end

  module State = struct
    type t =
      | Waiting_for_start
      | Waiting_for_length
      | Streaming_in
      | Flushing
    [@@deriving sexp, enumerate, compare]
  end

  let create
    (scope : Scope.t)
    ({ I.clock; clear; in_valid; in_data; out = { ready = out_ready } } : _ I.t)
    =
    let ( -- ) = Scope.naming scope in
    let reg_spec = Reg_spec.create ~clock ~clear () in
    let reg_spec_no_clear = Reg_spec.create ~clock () in
    let out = P.Contents_stream.Tx.Of_always.wire zero in
    let wout = width out.data.data.value in
    if wout <> Config.serial_input_width
    then
      raise_s
        [%message
          "ERROR: We do not currently support packets with larger data buses than the \
           serial input bus"];
    let state = State_machine.create (module State) reg_spec in
    ignore (state.current -- "current_state" : Signal.t);
    let should_write_packet_buffer = Variable.wire ~default:(zero 1) in
    let reading_packet_buffer = Variable.wire ~default:(zero 1) in
    let reading_length = Variable.reg ~width:16 reg_spec_no_clear in
    let which_length_packet = Variable.reg ~width:4 reg_spec_no_clear in
    let num_length_packets =
      let wserial = width in_data in
      let wlen = width reading_length.value in
      if wlen % wserial <> 0 then (wlen / wserial) + 1 else wlen / wserial
    in
    let packet_buffer =
      Fifo.create
        ~showahead:true
        ~capacity:Config.max_packet_length_in_data_widths
        ~clock
        ~clear
        ~wr:(should_write_packet_buffer.value -- "wr_enable")
        ~d:(in_data -- "wr_data")
        ~rd:(reading_packet_buffer.value -- "rd_data")
        ~scope:(Scope.sub_scope scope "fifo")
        ()
    in
    let have_buffered_packets = ~:(packet_buffer.empty) -- "have_buffered_packets" in
    compile
      [ reading_packet_buffer <-- (have_buffered_packets &: out_ready)
      ; state.switch
          [ ( State.Waiting_for_start
            , [ which_length_packet <--. 0
              ; when_
                  (in_valid &: (in_data ==: of_char Config.magic))
                  [ state.set_next Waiting_for_length ]
              ] )
          ; ( Waiting_for_length
            , [ when_
                  in_valid
                  [ which_length_packet <-- which_length_packet.value +:. 1
                  ; reading_length
                    <-- mux_init
                          which_length_packet.value
                          ~f:(fun which_length_packet_i ->
                            let current_parts =
                              split_msb
                                ~part_width:Config.serial_input_width
                                reading_length.value
                            in
                            concat_msb
                              (List.take current_parts which_length_packet_i
                               @ [ in_data ]
                               @ List.drop current_parts (which_length_packet_i + 1)))
                          num_length_packets
                  ; when_
                      (which_length_packet.value ==:. num_length_packets - 1)
                      [ state.set_next Streaming_in ]
                  ]
              ] )
          ; ( Streaming_in
            , [ when_
                  in_valid
                  [ should_write_packet_buffer <--. 1
                  ; reading_length <-- reading_length.value -:. 1
                  ; when_ (reading_length.value ==:. 1) [ state.set_next Flushing ]
                  ]
              ] )
          ; ( Flushing
            , [ when_ ~:have_buffered_packets [ state.set_next Waiting_for_start ] ] )
          ]
      ];
    { O.out =
        { P.Contents_stream.Tx.valid = have_buffered_packets
        ; data =
            { data = packet_buffer.q
            ; last =
                state.is State.Flushing
                &: (packet_buffer.used -- "packet_buffer_used" ==:. 1)
            }
        }
    }
  ;;

  let hierarchical ~instance (scope : Scope.t) (input : Signal.t I.t) =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ~scope ~name:"Serial_to_packet" ~instance create input
  ;;
end
