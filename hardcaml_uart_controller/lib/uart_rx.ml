(** This implements an 8-bit UART receiver at a fixed baud rate given a
    fixed input frequency. The functor specifies whether to expect and validate
    a parity bit and a stop bit. *)
open! Core

open Hardcaml
open Signal
open Always

module Make (Config : sig
    val clock_frequency : int
    val baud_rate : int
    val include_parity_bit : bool
    val stop_bits : int
  end) =
struct
  let switching_frequency = Config.clock_frequency / Config.baud_rate

  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; uart_rx : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t =
      { data_out_valid : 'a
      ; data_out : 'a [@bits 8]
      }
    [@@deriving sexp_of, hardcaml]
  end

  module State = struct
    type t =
      | Waiting_for_start_bit
      | Waiting_for_data_bits
      | Waiting_for_parity_bit
      | Waiting_for_stop_bits
    [@@deriving sexp, enumerate, compare]
  end

  let switch_cycle spec =
    let bits_to_repr_switching_frequency = Int.ceil_log2 switching_frequency in
    (reg_fb ~width:bits_to_repr_switching_frequency ~f:(fun t ->
       mod_counter ~max:(switching_frequency - 1) t))
      spec
    ==:. 0
  ;;

  let create (scope : Scope.t) ({ I.clock; clear; uart_rx = _ } : _ I.t) =
    let ( -- ) = Scope.naming scope in
    let reg_spec = Reg_spec.create ~clock ~clear () in
    let current_state = State_machine.create (module State) reg_spec in
    let switch_cycle = switch_cycle reg_spec in
    ignore (current_state.current -- "current_state" : Signal.t);
    compile
      [ current_state.switch
          [ State.Waiting_for_start_bit, [ when_ switch_cycle [] ]
          ; State.Waiting_for_data_bits, [ when_ switch_cycle [] ]
          ; State.Waiting_for_parity_bit, [ when_ switch_cycle [] ]
          ; State.Waiting_for_stop_bits, [ when_ switch_cycle [] ]
          ]
      ];
    { O.data_out_valid = assert false; data_out = assert false }
  ;;

  let hierarchical ~instance (scope : Scope.t) (input : Signal.t I.t) =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ~scope ~name:"Uart_tx" ~instance create input
  ;;
end
