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
    [@@deriving sexp_of, hardcaml ~rtlmangle:"$"]
  end

  module O = struct
    type 'a t =
      { uart_tx : 'a
      ; data_in_ready : 'a
      ; idle : 'a
      }
    [@@deriving sexp_of, hardcaml ~rtlmangle:"$"]
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

  let switch_cycle ~reset_when spec =
    if switching_frequency = 1
    then vdd
    else (
      let bits_to_repr_switching_frequency = Int.ceil_log2 switching_frequency in
      (reg_fb ~width:bits_to_repr_switching_frequency ~f:(fun t ->
         mux2
           reset_when
           (one bits_to_repr_switching_frequency)
           (mod_counter ~max:(switching_frequency - 1) t)))
        spec
      ==:. 0)
  ;;

  let create (scope : Scope.t) ({ I.clock; clear; data_in_valid; data_in } : _ I.t) =
    let ( -- ) = Scope.naming scope in
    let reg_spec = Reg_spec.create ~clock ~clear () in
    let reg_spec_no_clear = Reg_spec.create ~clock () in
    let current_state = State_machine.create (module State) reg_spec in
    (* When we are transmitting we hold our output at the value stored in a
       register. *)
    let current_output_wire = Variable.wire ~default:gnd in
    let reset_switch_cycle = Variable.wire ~default:gnd in
    let current_output_reg = Variable.reg ~width:1 reg_spec_no_clear in
    let switch_cycle =
      switch_cycle ~reset_when:reset_switch_cycle.value reg_spec_no_clear
      -- "switch_cycle"
    in
    let data_to_write = Variable.reg ~width:(width data_in) reg_spec_no_clear in
    let which_data_bits = Variable.reg ~width:3 reg_spec_no_clear in
    let parity_bit = Variable.reg ~width:1 reg_spec_no_clear in
    let which_stop_bit = Variable.reg ~width:2 reg_spec_no_clear in
    let next_data_bit =
      mux which_data_bits.value (split_lsb ~part_width:1 data_to_write.value)
      -- "next_data_bit"
    in
    ignore (current_state.current -- "current_state" : Signal.t);
    (* TODO: Get rid of the current output wire and just delay tx by a cycle
       with a reg to simplify the code. *)
    compile
      [ current_state.switch
          [ ( State.Waiting_for_data_in
            , [ current_output_wire <--. 1
              ; parity_bit <--. 0
              ; which_stop_bit <--. 0
              ; which_data_bits <--. 0
              ; reset_switch_cycle <-- vdd
              ; when_
                  data_in_valid
                  [ data_to_write <-- data_in
                  ; current_state.set_next Waiting_for_start_bit
                  ]
              ] )
          ; ( State.Waiting_for_start_bit
            , [ current_output_wire <-- current_output_reg.value
              ; when_
                  switch_cycle
                  [ current_state.set_next State.Waiting_for_data_bits
                  ; current_output_reg <--. 0
                  ; current_output_wire <--. 0
                  ]
              ] )
          ; ( State.Waiting_for_data_bits
            , [ current_output_wire <-- current_output_reg.value
              ; when_
                  switch_cycle
                  [ current_output_reg <-- next_data_bit
                  ; current_output_wire <-- next_data_bit
                  ; parity_bit <-- parity_bit.value +: next_data_bit
                  ; which_data_bits <-- which_data_bits.value +:. 1
                  ; when_
                      (which_data_bits.value ==:. 7)
                      (if C.config.include_parity_bit
                       then [ current_state.set_next Waiting_for_parity_bit ]
                       else [ current_state.set_next Waiting_for_stop_bits ])
                  ]
              ] )
          ; ( State.Waiting_for_parity_bit
            , [ current_output_wire <-- current_output_reg.value
              ; when_
                  switch_cycle
                  [ current_output_reg <-- parity_bit.value -- "parity_bit"
                  ; current_output_wire <-- parity_bit.value
                  ; current_state.set_next Waiting_for_stop_bits
                  ]
              ] )
          ; ( State.Waiting_for_stop_bits
            , [ current_output_wire <-- current_output_reg.value
              ; when_
                  switch_cycle
                  [ which_stop_bit <-- which_stop_bit.value -- "which_stop_bit" +:. 1
                  ; current_output_reg <--. 1
                  ; (* While unlikely in practice, if the baud rate is equal to
                       clock rate then just setting the register would be wrong as
                       it would only reflect the change on the next cycle. *)
                    current_output_wire <--. 1
                  ; when_
                      (which_stop_bit.value ==:. C.config.stop_bits - 1)
                      [ current_state.set_next Waiting_for_data_in ]
                  ]
              ] )
          ]
      ];
    { O.uart_tx = current_output_wire.value
    ; data_in_ready = current_state.is State.Waiting_for_data_in
    ; idle = current_state.is Waiting_for_data_in
    }
  ;;

  let hierarchical ~instance (scope : Scope.t) (input : Signal.t I.t) =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ~scope ~name:"uart_tx" ~instance create input
  ;;
end
