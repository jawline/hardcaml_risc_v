(** This implements an 8-bit UART transmitter at a fixed baud rate given a
    fixed input frequency.

    Format: [start bit][data bits][parity bit][end bit(s)]

    At other times the UART line is held high. *)
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
      ; data_in_valid : 'a
      ; data_in : 'a [@bits 8]
      }
    [@@deriving hardcaml ~rtlmangle:"$"]
  end

  module O = struct
    type 'a t =
      { uart_tx : 'a
      ; data_in_ready : 'a
      ; idle : 'a
      }
    [@@deriving hardcaml ~rtlmangle:"$"]
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
    let width = num_bits_to_represent (switching_frequency - 1) in
    (reg_fb ~width ~clear_to:(zero width) ~f:(fun t ->
       mod_counter ~max:(switching_frequency - 1) t))
      spec
    ==:. switching_frequency - 1
  ;;

  let create (scope : Scope.t) ({ I.clock; clear; data_in_valid; data_in } : _ I.t) =
    let ( -- ) = Scope.naming scope in
    let reg_spec = Reg_spec.create ~clock ~clear () in
    let reg_spec_no_clear = Reg_spec.create ~clock () in
    let current_state = State_machine.create (module State) reg_spec in
    let current_output_reg = Variable.reg ~width:1 ~clear_to:vdd reg_spec_no_clear in
    (* When we are transmitting we hold our output at the value stored in a
       register. *)
    let reg_spec_clear_on_reset_switch_cycle =
      Reg_spec.create ~clock ~clear:(current_state.is Waiting_for_data_in) ()
    in
    let%hw switch_cycle = switch_cycle reg_spec_clear_on_reset_switch_cycle in
    let%hw_var data_to_write = Variable.reg ~width:(width data_in) reg_spec_no_clear in
    let%hw_var which_data_bits = Variable.reg ~width:3 reg_spec_no_clear in
    let%hw_var parity_bit = Variable.reg ~width:1 reg_spec_no_clear in
    let%hw_var which_stop_bit = Variable.reg ~width:2 reg_spec_no_clear in
    let next_data_bit =
      mux which_data_bits.value (split_lsb ~part_width:1 data_to_write.value)
    in
    ignore (current_state.current -- "current_state" : Signal.t);
    (* TODO: Get rid of the current output wire and just delay tx by a cycle
       with a reg to simplify the code. *)
    let stop_bit =
      proc [ current_output_reg <-- vdd; current_state.set_next Waiting_for_stop_bits ]
    in
    let data_bit =
      proc
        [ current_state.set_next Waiting_for_data_bits
        ; current_output_reg <-- next_data_bit
        ; parity_bit <-- parity_bit.value +: next_data_bit
        ; incr which_data_bits
        ]
    in
    compile
      [ current_state.switch
          [ ( State.Waiting_for_data_in
            , [ current_output_reg <-- vdd
              ; parity_bit <--. 0
              ; which_stop_bit <--. 0
              ; which_data_bits <--. 0
              ; when_
                  data_in_valid
                  [ data_to_write <-- data_in
                  ; current_output_reg <-- gnd
                  ; current_state.set_next Waiting_for_start_bit
                  ]
              ] )
          ; State.Waiting_for_start_bit, [ when_ switch_cycle [ data_bit ] ]
          ; ( State.Waiting_for_data_bits
            , [ when_
                  switch_cycle
                  [ data_bit
                  ; when_
                      (which_data_bits.value ==:. 0)
                      (if C.config.include_parity_bit
                       then
                         [ current_output_reg <-- parity_bit.value
                         ; current_state.set_next Waiting_for_parity_bit
                         ]
                       else [ stop_bit ])
                  ]
              ] )
          ; State.Waiting_for_parity_bit, [ when_ switch_cycle [ stop_bit ] ]
          ; ( State.Waiting_for_stop_bits
            , [ when_
                  switch_cycle
                  [ incr which_stop_bit
                  ; when_
                      (which_stop_bit.value ==:. C.config.stop_bits - 1)
                      [ current_state.set_next Waiting_for_data_in ]
                  ]
              ] )
          ]
      ];
    { O.uart_tx = current_output_reg.value
    ; data_in_ready = current_state.is State.Waiting_for_data_in
    ; idle = current_state.is Waiting_for_data_in
    }
  ;;

  let hierarchical ~instance (scope : Scope.t) (input : Signal.t I.t) =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ~scope ~name:"uart_tx" ~instance create input
  ;;
end
