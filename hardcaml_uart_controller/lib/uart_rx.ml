(** This implements an 8-bit UART receiver at a fixed baud rate given a
    fixed input frequency. The functor specifies whether to expect and validate
    a parity bit and a stop bit. *)
open! Core

open Hardcaml
open Signal
open Always

module Make (C : Config_intf.S) = struct
  let switching_frequency = C.config.clock_frequency / C.config.baud_rate

  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; uart_rx : 'a
      }
    [@@deriving sexp_of, hardcaml ~rtlmangle:true]
  end

  module O = struct
    type 'a t =
      { data_out_valid : 'a
      ; data_out : 'a [@bits 8]
      ; parity_error : 'a
      ; stop_bit_unstable : 'a
      }
    [@@deriving sexp_of, hardcaml ~rtlmangle:true]
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
    if switching_frequency = 1
    then vdd
    else (
      let bits_to_repr_switching_frequency = Int.ceil_log2 switching_frequency in
      (reg_fb ~width:bits_to_repr_switching_frequency ~f:(fun t ->
         mod_counter ~max:(switching_frequency - 1) t))
        spec
      ==:. 0)
  ;;

  let create (scope : Scope.t) ({ I.clock; clear; uart_rx } : _ I.t) =
    let ( -- ) = Scope.naming scope in
    let reg_spec = Reg_spec.create ~clock ~clear () in
    let reg_spec_no_clear = Reg_spec.create ~clock () in
    let current_state = State_machine.create (module State) reg_spec in
    let switch_cycle = switch_cycle reg_spec_no_clear -- "switch_cycle" in
    let data = Variable.reg ~width:8 reg_spec_no_clear in
    let which_data_bit = Variable.reg ~width:3 reg_spec_no_clear in
    let which_stop_bit = Variable.reg ~width:2 reg_spec_no_clear in
    (* Data with which_data_bit replaced with the current uart_rx *)
    let data_with_new_data_bit =
      mux_init
        ~f:(fun index ->
          let bits = split_lsb ~part_width:1 data.value in
          concat_lsb (List.take bits index @ [ uart_rx ] @ List.drop bits (index + 1)))
        which_data_bit.value
        8
      -- "data_with_new_bit"
    in
    (* The parity bit should always = the RX parity bit if Config.include_parity_bit is set *)
    let parity_bit = Variable.reg ~width:1 reg_spec_no_clear in
    let rx_parity_bit = Variable.reg ~width:1 reg_spec_no_clear in
    let parity_bit_matches =
      if C.config.include_parity_bit
      then
        parity_bit.value -- "calculated_parity_bit"
        ==: rx_parity_bit.value -- "rx_parity_bit"
      else vdd
    in
    let stop_bit_not_stable = Variable.reg ~width:1 reg_spec_no_clear in
    ignore (current_state.current -- "current_state" : Signal.t);
    let data_out_valid = Variable.wire ~default:gnd in
    let parity_error = Variable.wire ~default:gnd in
    compile
      [ current_state.switch
          [ ( State.Waiting_for_start_bit
            , [ data <--. 0
              ; parity_bit <--. 0
              ; rx_parity_bit <--. 0
              ; which_data_bit <--. 0
              ; stop_bit_not_stable <--. 0
              ; which_stop_bit <--. 0
              ; when_
                  (switch_cycle &: (uart_rx ==:. 0))
                  [ current_state.set_next State.Waiting_for_data_bits ]
              ] )
          ; ( State.Waiting_for_data_bits
            , [ when_
                  switch_cycle
                  [ parity_bit <-- parity_bit.value +: uart_rx
                  ; data <-- data_with_new_data_bit
                  ; which_data_bit <-- which_data_bit.value +:. 1
                  ; when_
                      (which_data_bit.value -- "which_data_bit" ==:. 7)
                      [ (if C.config.include_parity_bit
                         then current_state.set_next Waiting_for_parity_bit
                         else current_state.set_next Waiting_for_stop_bits)
                      ]
                  ]
              ] )
          ; ( State.Waiting_for_parity_bit
            , [ when_
                  switch_cycle
                  [ rx_parity_bit <-- uart_rx
                  ; current_state.set_next Waiting_for_stop_bits
                  ]
              ] )
          ; ( State.Waiting_for_stop_bits
            , [ when_
                  switch_cycle
                  [ which_stop_bit <-- which_stop_bit.value +:. 1
                  ; when_ (uart_rx ==:. 0) [ stop_bit_not_stable <--. 1 ]
                  ; when_
                      (which_stop_bit.value ==:. C.config.stop_bits - 1)
                      [ data_out_valid
                        <-- (~:(stop_bit_not_stable.value) &: parity_bit_matches)
                      ; parity_error <-- ~:parity_bit_matches
                      ; current_state.set_next Waiting_for_start_bit
                      ]
                  ]
              ] )
          ]
      ];
    { O.data_out_valid = data_out_valid.value
    ; parity_error = parity_error.value
    ; stop_bit_unstable = stop_bit_not_stable.value
    ; data_out = data.value
    }
  ;;

  let hierarchical ~instance (scope : Scope.t) (input : Signal.t I.t) =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ~scope ~name:"uart_rx" ~instance create input
  ;;
end
