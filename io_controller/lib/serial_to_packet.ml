open! Core
open Hardcaml
open Signal
open! Always

(* This expects packets from a serial port in the form [ header ; length MSB ;
   length LSB ; data ... ]. If a second packet is received before this packet
   is flushed out then it will be dropped so the serial sender is responsible
   for waiting for acknowledgement. *)

(* TODO: Consider adding a checksum. *)

module Make
    (Config : sig
       val header : char
       val serial_input_width : int
     end)
    (P : Packet_intf.S) =
struct
  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; in_valid : 'a
      ; in_data : 'a [@bits Config.serial_input_width]
      ; out : 'a P.Contents_stream.Rx.t
      }
    [@@deriving hardcaml ~rtlmangle:"$"]
  end

  module O = struct
    type 'a t =
      { out : 'a P.Contents_stream.Tx.t
      ; ready : 'a
      }
    [@@deriving hardcaml ~rtlmangle:"$"]
  end

  module State = struct
    type t =
      | Waiting_for_start
      | Waiting_for_length
      | Streaming_in
    [@@deriving sexp, enumerate, compare]
  end

  let output_width = P.Contents_stream.Tx.port_widths.data.data

  let () =
    if output_width <> Config.serial_input_width
    then
      raise_s
        [%message
          "ERROR: We do not currently support packets with larger data buses than the \
           serial input bus"]
  ;;

  let create
    (scope : Scope.t)
    ({ I.clock; clear; in_valid; in_data; out = { ready = out_ready } } : _ I.t)
    =
    let ( -- ) = Scope.naming scope in
    let reg_spec = Reg_spec.create ~clock ~clear () in
    let state = State_machine.create (module State) reg_spec in
    ignore (state.current -- "current_state" : Signal.t);
    let reading_length = Variable.reg ~width:16 reg_spec in
    let which_length_packet = Variable.reg ~width:4 reg_spec in
    let num_length_packets =
      let wserial = width in_data in
      let wlen = width reading_length.value in
      if wlen % wserial <> 0 then (wlen / wserial) + 1 else wlen / wserial
    in
    let length_this_cycle =
      mux_init
        which_length_packet.value
        ~f:(fun which_length_packet_i ->
          let current_parts =
            split_msb ~part_width:Config.serial_input_width reading_length.value
          in
          let new_length =
            concat_msb
              (List.take current_parts which_length_packet_i
               @ [ in_data ]
               @ List.drop current_parts (which_length_packet_i + 1))
          in
          new_length)
        num_length_packets
    in
    compile
      [ state.switch
          [ ( State.Waiting_for_start
            , [ which_length_packet <--. 0
              ; when_
                  (in_valid &: (in_data ==: of_char Config.header))
                  [ state.set_next Waiting_for_length ]
              ] )
          ; ( Waiting_for_length
            , [ when_
                  in_valid
                  [ incr which_length_packet
                  ; reading_length <-- length_this_cycle
                  ; when_
                      (which_length_packet.value ==:. num_length_packets - 1)
                      [ (* Zero length packets are disallowed *)
                        if_
                          (length_this_cycle ==:. 0)
                          [ state.set_next Waiting_for_start ]
                          [ state.set_next Streaming_in ]
                      ]
                  ]
              ] )
          ; ( Streaming_in
            , [ when_
                  (in_valid &: out_ready)
                  [ decr reading_length
                  ; when_
                      (reading_length.value ==:. 1)
                      [ state.set_next Waiting_for_start ]
                  ]
              ] )
          ]
      ];
    { O.out =
        { P.Contents_stream.Tx.valid = in_valid &: state.is Streaming_in
        ; data = { data = in_data; last = reading_length.value ==:. 1 }
        }
    ; ready =
        state.is Waiting_for_start
        |: state.is Waiting_for_length
        |: (state.is Streaming_in &: out_ready)
    }
  ;;

  let hierarchical ~instance (scope : Scope.t) (input : Signal.t I.t) =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ~scope ~name:"serial_to_packet" ~instance create input
  ;;
end
