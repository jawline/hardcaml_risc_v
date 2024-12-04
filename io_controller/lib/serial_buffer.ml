open! Core
open Hardcaml
open Signal
open! Always

module Make (Config : sig
    val serial_input_width : int
  end) =
struct
  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; in_valid : 'a
      ; in_data : 'a [@bits Config.serial_input_width]
      ; out_ready : 'a
      }
    [@@deriving hardcaml ~rtlmangle:"$"]
  end

  module O = struct
    type 'a t =
      { out_valid : 'a
      ; out_data : 'a [@bits Config.serial_input_width]
      }
    [@@deriving hardcaml ~rtlmangle:"$"]
  end

  let create
    ~capacity
    (_scope : Scope.t)
    ({ I.clock; clear; in_valid; in_data; out_ready } : _ I.t)
    =
    let buffer =
      Fifo.create
        ~showahead:true
        ~capacity
        ~clock
        ~clear
        ~wr:in_valid
        ~d:in_data
        ~rd:out_ready
        ()
    in
    { O.out_valid = ~:(buffer.empty); out_data = buffer.q }
  ;;

  let hierarchical ~instance ~capacity (scope : Scope.t) (input : Signal.t I.t) =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ~scope ~name:"serial_buffer" ~instance (create ~capacity) input
  ;;
end
