(** This implements an 8-bit UART transmitter at a fixed baud rate given a
    fixed input frequency.

    Format: [start bit][data bits][parity bit][end bit(s)]

    At other times the UART line is held high. *)
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
      ; data_in_valid : 'a
      ; data_in : 'a [@bits 8]
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t =
      { uart_tx : 'a
      ; data_in_ready : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  module State = struct
    type t =
      | Waiting_for_data_in
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

  let create (scope : Scope.t) ({ I.clock; clear; data_in_valid; data_in } : _ I.t) =
    let ( -- ) = Scope.naming scope in
    let reg_spec = Reg_spec.create ~clock ~clear () in
    let reg_spec_no_clear = Reg_spec.create ~clock () in
    let current_state = State_machine.create (module State) reg_spec in
    let current_output = Variable.reg ~width:1 reg_spec_no_clear in
    let switch_cycle = switch_cycle reg_spec_no_clear -- "switch_cycle" in
    let data_to_write = Variable.reg ~width:(width data_in) reg_spec_no_clear in
    let which_data_bits = Variable.reg ~width:3 reg_spec_no_clear in
    let parity_bit = Variable.reg ~width:1 reg_spec_no_clear in
    let which_stop_bit = Variable.reg ~width:2 reg_spec_no_clear in
    let next_data_bit =
      mux_init
        ~f:(fun index_signal -> select data_to_write.value index_signal index_signal)
        which_data_bits.value
        8
    in
    ignore (current_state.current -- "current_state" : Signal.t);
    compile
      [ current_state.switch
          [ ( State.Waiting_for_data_in
            , [ current_output <--. 1
              ; parity_bit <--. 0
              ; which_stop_bit <--. 0
              ; which_data_bits <--. 0
              ; when_
                  data_in_valid
                  [ data_to_write <-- data_in
                  ; current_state.set_next Waiting_for_start_bit
                  ]
              ] )
          ; ( State.Waiting_for_start_bit
            , [ when_
                  switch_cycle
                  [ current_state.set_next State.Waiting_for_data_bits
                  ; current_output <--. 0
                  ]
              ] )
          ; ( State.Waiting_for_data_bits
            , [ when_
                  switch_cycle
                  [ current_output <-- next_data_bit
                  ; parity_bit <-- parity_bit.value +: next_data_bit
                  ; which_data_bits <-- which_data_bits.value +:. 1
                  ; when_
                      (which_data_bits.value ==:. 7)
                      (if Config.include_parity_bit
                       then [ current_state.set_next Waiting_for_parity_bit ]
                       else [ current_state.set_next Waiting_for_stop_bits ])
                  ]
              ] )
          ; ( State.Waiting_for_parity_bit
            , [ when_
                  switch_cycle
                  [ current_output <-- parity_bit.value
                  ; current_state.set_next Waiting_for_stop_bits
                  ]
              ] )
          ; ( State.Waiting_for_stop_bits
            , [ when_
                  switch_cycle
                  [ which_stop_bit <-- which_stop_bit.value +:. 1
                  ; current_output <--. 1
                  ; when_
                      (which_stop_bit.value ==:. Config.stop_bits)
                      [ current_state.set_next Waiting_for_data_in ]
                  ]
              ] )
          ]
      ];
    { O.uart_tx = current_output.value
    ; data_in_ready = current_state.is State.Waiting_for_data_in
    }
  ;;

  let hierarchical ~instance (scope : Scope.t) (input : Signal.t I.t) =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ~scope ~name:"Uart_tx" ~instance create input
  ;;
end
