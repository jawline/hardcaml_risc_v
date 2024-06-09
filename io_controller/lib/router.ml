open! Core
open Hardcaml
open Signal
open! Always

(* TODO: Tests *)
(* This expects packets with a leading tag byte that we use to route to one of
   several output streams. *)

module Make
    (Config : sig
       val num_tags : int
     end)
    (P : Packet_intf.S) =
struct
  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; in_ : 'a P.Contents_stream.Tx.t
      ; outs : 'a P.Contents_stream.Rx.t list [@length Config.num_tags]
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t =
      { in_ : 'a P.Contents_stream.Rx.t
      ; outs : 'a P.Contents_stream.Tx.t list [@length Config.num_tags]
      }
    [@@deriving sexp_of, hardcaml]
  end

  module State = struct
    type t =
      | Waiting_for_start_of_packet
      | Routing
      | Discarding_bad_tag
    [@@deriving sexp, enumerate, compare]
  end

  let create (scope : Scope.t) ({ I.clock; clear; in_; outs } : _ I.t) =
    let ( -- ) = Scope.naming scope in
    let reg_spec = Reg_spec.create ~clock ~clear () in
    let reg_spec_no_clear = Reg_spec.create ~clock () in
    let state = State_machine.create (module State) reg_spec in
    ignore (state.current -- "current_state" : Signal.t);
    let which_tag =
      Variable.reg
        ~width:(Signal.num_bits_to_represent (Config.num_tags - 1))
        reg_spec_no_clear
    in
    let selected_out_ready =
      let output_is_ready =
        if Config.num_tags = 1
        then (List.hd_exn outs).ready
        else mux which_tag.value (List.map ~f:(fun out -> out.ready) outs)
      in
      state.is Routing
      &: output_is_ready
      |: state.is Waiting_for_start_of_packet
      |: state.is Discarding_bad_tag
    in
    compile
      [ state.switch
          [ ( State.Waiting_for_start_of_packet
            , [ which_tag <-- uresize in_.data.data (width which_tag.value)
              ; when_
                  in_.valid
                  [ (* If a received tag is out of the range of the
                       routable tags we discard the whole packet in the
                       router. *)
                    when_
                      ~:(in_.data.last)
                      [ if_
                          (in_.data.data <=:. Config.num_tags - 1)
                          [ state.set_next Routing ]
                          [ state.set_next Discarding_bad_tag ]
                      ]
                  ]
              ] )
          ; ( Routing
            , [ when_ in_.data.last [ state.set_next Waiting_for_start_of_packet ] ] )
          ; ( Discarding_bad_tag
            , [ when_ in_.data.last [ state.set_next Waiting_for_start_of_packet ] ] )
          ]
      ];
    { O.in_ = { ready = selected_out_ready }
    ; outs =
        List.init
          ~f:(fun index ->
            P.Contents_stream.Tx.Of_signal.mux2
              (which_tag.value -- "which_tag" ==:. index &: state.is Routing)
              in_
              (P.Contents_stream.Tx.Of_signal.of_int 0))
          Config.num_tags
    }
  ;;

  let hierarchical ~instance (scope : Scope.t) (input : Signal.t I.t) =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ~scope ~name:"Packet_router" ~instance create input
  ;;
end
