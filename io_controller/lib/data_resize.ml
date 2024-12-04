open! Core
open Hardcaml
open Signal
open! Always

(* This expects packets with a leading tag byte that we use to route to one of
   several output streams. *)

module Make (Config : sig
    val input_width : int
    val output_width : int
  end) =
struct
  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; in_valid : 'a
      ; in_data : 'a [@bits Config.input_width]
      ; out_ready : 'a
      }
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t =
      { out_valid : 'a
      ; out_data : 'a [@bits Config.output_width]
      ; interim_data_buffered : 'a
      ; ready : 'a
      }
    [@@deriving hardcaml]
  end

  module State = struct
    type t =
      | Buffer
      | Flush
    [@@deriving sexp, enumerate, compare]
  end

  let () =
    if Config.output_width % Config.input_width <> 0
    then raise_s [%message "BUG: Output must be a multiple of input width"]
  ;;

  let buffer_beats = Config.output_width / Config.input_width

  let create (scope : Scope.t) ({ I.clock; clear; in_valid; in_data; out_ready } : _ I.t) =
    let ( -- ) = Scope.naming scope in
    let reg_spec = Reg_spec.create ~clock ~clear () in
    let state = State_machine.create (module State) reg_spec in
    ignore (state.current -- "current_state" : Signal.t);
    let ctr = Variable.reg ~width:(num_bits_to_represent (buffer_beats - 1)) reg_spec in
    let data_parts =
      List.init ~f:(fun _ -> Variable.reg ~width:Config.input_width reg_spec) buffer_beats
    in
    compile
      [ state.switch
          [ ( State.Buffer
            , [ when_
                  in_valid
                  [ ctr <-- mod_counter ~max:(buffer_beats - 1) ctr.value
                  ; when_ (ctr.value ==:. buffer_beats - 1) [ state.set_next Flush ]
                  ; (* Assign this cycles data part. *)
                    List.mapi
                      ~f:(fun cycle part ->
                        part <-- mux2 (ctr.value ==:. cycle) in_data part.value)
                      data_parts
                    |> proc
                  ]
              ] )
          ; Flush, [ when_ out_ready [ state.set_next Buffer ] ]
          ]
      ];
    { O.out_valid = state.is Flush
    ; out_data = concat_lsb (List.map ~f:(fun t -> t.value) data_parts)
    ; interim_data_buffered = ctr.value <>:. 0
    ; ready = state.is Buffer
    }
  ;;

  let hierarchical ~instance (scope : Scope.t) (input : Signal.t I.t) =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ~scope ~name:"packet_router" ~instance create input
  ;;
end
