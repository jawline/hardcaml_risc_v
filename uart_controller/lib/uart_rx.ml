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
    [@@deriving hardcaml ~rtlmangle:"$"]
  end

  module O = struct
    type 'a t =
      { data_out_valid : 'a
      ; data_out : 'a [@bits 8]
      ; parity_error : 'a
      }
    [@@deriving hardcaml ~rtlmangle:"$"]
  end

  module State = struct
    type t =
      | Idle
      | Waiting_for_start_bit
      | Waiting_for_data_bits
      | Waiting_for_parity_bit
      | Waiting_for_stop_bits
    [@@deriving sexp, enumerate, compare]
  end

  type 'a switches =
    { bit : 'a
    ; half : 'a
    }

  let switch_cycle ~frequency spec =
    let width = num_bits_to_represent (frequency - 1) in
    let ctr =
      (reg_fb ~width ~clear_to:(one width) ~f:(fun t ->
         mod_counter ~max:(frequency - 1) t))
        spec
    in
    { bit = ctr ==:. frequency - 1; half = ctr ==:. (frequency / 2) - 1 }
  ;;

  let create (scope : Scope.t) ({ I.clock; clear; uart_rx } : _ I.t) =
    let ( -- ) = Scope.naming scope in
    let reg_spec = Reg_spec.create ~clock ~clear () in
    let reg_spec_no_clear = Reg_spec.create ~clock () in
    let uart_rx =
      pipeline ~n:3 reg_spec_no_clear uart_rx
      |: pipeline ~n:2 reg_spec_no_clear uart_rx
      |: pipeline ~n:1 reg_spec_no_clear uart_rx
    in
    let current_state = State_machine.create (module State) reg_spec in
    let clear_switch_counters = Variable.wire ~default:gnd in
    assert (switching_frequency > 1);
    let { bit = full_bit; half = half_bit } =
      switch_cycle
        ~frequency:switching_frequency
        (Reg_spec.create ~clock ~clear:clear_switch_counters.value ())
    in
    let%hw full_bit = full_bit in
    let%hw half_bit = half_bit in
    let%hw_var data = Variable.reg ~width:8 reg_spec_no_clear in
    let%hw_var which_data_bit = Variable.reg ~width:3 reg_spec_no_clear in
    (* Data with which_data_bit replaced with the current uart_rx *)
    let data_with_new_data_bit =
      mux_init
        ~f:(fun index ->
          let bits = split_lsb ~part_width:1 data.value in
          concat_lsb (List.take bits index @ [ uart_rx ] @ List.drop bits (index + 1)))
        which_data_bit.value
        8
    in
    (* The parity bit should always = the RX parity bit if Config.include_parity_bit is set *)
    let parity_bit = Variable.reg ~width:1 reg_spec_no_clear in
    let rx_parity_bit = Variable.reg ~width:1 reg_spec_no_clear in
    let parity_bit_matches =
      if C.config.include_parity_bit
      then parity_bit.value ==: rx_parity_bit.value
      else vdd
    in
    ignore (current_state.current -- "current_state" : Signal.t);
    let%hw_var data_out_valid = Variable.wire ~default:gnd in
    let%hw_var parity_error = Variable.wire ~default:gnd in
    let%hw uart_rx_negative_edge = reg reg_spec_no_clear uart_rx &: ~:uart_rx in
    compile
      [ current_state.switch
          [ ( State.Idle
            , [ data <--. 0
              ; parity_bit <--. 0
              ; rx_parity_bit <--. 0
              ; which_data_bit <--. 0
              ; when_
                  uart_rx_negative_edge
                  [ clear_switch_counters <-- vdd
                  ; current_state.set_next State.Waiting_for_start_bit
                  ]
              ] )
          ; ( Waiting_for_start_bit
            , [ when_ full_bit [ current_state.set_next Waiting_for_data_bits ] ] )
          ; ( Waiting_for_data_bits
            , [ when_
                  half_bit
                  [ parity_bit <-- parity_bit.value +: uart_rx
                  ; data <-- data_with_new_data_bit
                  ]
              ; when_
                  full_bit
                  [ incr which_data_bit
                  ; when_
                      (which_data_bit.value ==:. 7)
                      [ (if C.config.include_parity_bit
                         then current_state.set_next Waiting_for_parity_bit
                         else current_state.set_next Waiting_for_stop_bits)
                      ]
                  ]
              ] )
          ; ( Waiting_for_parity_bit
            , [ when_ half_bit [ rx_parity_bit <-- uart_rx ]
              ; when_ full_bit [ current_state.set_next Waiting_for_stop_bits ]
              ] )
          ; ( Waiting_for_stop_bits
            , [ when_
                  half_bit
                  [ data_out_valid <-- vdd
                  ; parity_error <-- ~:parity_bit_matches
                  ; current_state.set_next Idle
                  ]
              ] )
          ]
      ];
    { O.data_out_valid = data_out_valid.value
    ; parity_error = parity_error.value
    ; data_out = data.value
    }
  ;;

  let hierarchical ~instance (scope : Scope.t) (input : Signal.t I.t) =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ~scope ~name:"uart_rx" ~instance create input
  ;;
end
