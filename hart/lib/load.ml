(** The Load opcode implements LW, LH, LB and non sign extended variants LHU and LBU.
    We currently disallow non-width aligned loads (e.g, shorts at index 1 and words
    at indices 1, 2, 3). *)
open! Core

open Hardcaml
open Hardcaml_memory_controller
open! Signal
open Always

module Make (Hart_config : Hart_config_intf.S) (Memory : Memory_bus_intf.S) = struct
  let register_width = Register_width.bits Hart_config.register_width

  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; enable : 'a
      ; funct3 : 'a [@bits 3]
      ; address : 'a [@bits register_width]
      ; read_bus : 'a Memory.Read_bus.Rx.t [@rtlprefix "read$"]
      ; read_response : 'a Memory.Read_response.With_valid.t [@rtlprefix "read_response$"]
      }
    [@@deriving hardcaml ~rtlmangle:"$"]
  end

  module O = struct
    type 'a t =
      { new_rd : 'a [@bits register_width] [@rtlname "new_rd"]
      ; error : 'a
      ; finished : 'a
      ; read_bus : 'a Memory.Read_bus.Tx.t [@rtlprefix "read$"]
      }
    [@@deriving hardcaml ~rtlmangle:"$"]
  end

  module State = struct
    type t =
      | Idle
      | Waiting_for_memory_controller
      | Waiting_for_load
    [@@deriving sexp, enumerate, compare]
  end

  let create
    (scope : Scope.t)
    ({ I.clock; clear; enable; funct3; address; read_bus; read_response } : _ I.t)
    =
    (* TODO: We currently disallow loads that are not aligned on a {load width}
       boundary. We could support this by loading a second word and muxing the
       result at the cost of an extra load cycle.  *)
    let ( -- ) = Scope.naming scope in
    let reg_spec = Reg_spec.create ~clock ~clear () in
    let current_state = State_machine.create (module State) reg_spec in
    ignore (current_state.current -- "current_state" : Signal.t);
    let read_request = Memory.Read_bus.Tx.Of_always.wire zero in
    let%hw aligned_address =
      (* Mask the read address to a 4-byte alignment. *)
      address &: ~:(of_int ~width:register_width 0b11)
    in
    let%hw unaligned_bits =
      Util.switch
        (module Funct3.Load)
        ~if_not_found:(zero 2)
        ~f:(function
          | Funct3.Load.Lw -> sel_bottom ~width:2 address &:. 0b11
          | Lh | Lhu -> sel_bottom ~width:2 address &:. 0b1
          | Lb | Lbu -> zero 2)
        funct3
    in
    let is_unaligned = unaligned_bits <>:. 0 in
    let funct3_is_error =
      Util.switch (module Funct3.Load) ~if_not_found:vdd ~f:(fun _ -> gnd) funct3
    in
    let inputs_are_error = is_unaligned |: funct3_is_error in
    let issue_load =
      proc
        [ Memory.Read_bus.Tx.Of_always.assign
            read_request
            { valid = vdd; data = { address = aligned_address } }
        ; when_ read_bus.ready [ current_state.set_next Waiting_for_load ]
        ]
    in
    let finished = Variable.wire ~default:gnd in
    compile
      [ current_state.switch
          [ ( State.Idle
            , [ when_
                  enable
                  [ if_
                      inputs_are_error
                      [ finished <-- vdd ]
                      [ current_state.set_next Waiting_for_memory_controller; issue_load ]
                  ]
              ] )
          ; Waiting_for_memory_controller, [ issue_load ]
          ; ( Waiting_for_load
            , [ when_
                  read_response.valid
                  [ finished <-- vdd; current_state.set_next Idle ]
              ] )
          ]
      ];
    { O.new_rd =
        (let%hw alignment_bits = address &:. 0b11 in
         let%hw full_word = read_response.value.read_data in
         let%hw half_word =
           mux (alignment_bits <>:. 0) (split_lsb ~part_width:16 full_word)
         in
         let%hw byte = mux alignment_bits (split_lsb ~part_width:8 full_word) in
         Util.switch
           (module Funct3.Load)
           ~if_not_found:(zero register_width)
           ~f:(function
             | Funct3.Load.Lw -> full_word
             | Lh -> Decoder.sign_extend ~width:register_width half_word
             | Lhu -> uresize ~width:register_width half_word
             | Lb -> Decoder.sign_extend ~width:register_width byte
             | Lbu -> uresize ~width:register_width byte)
           funct3)
    ; error = read_response.valid &: read_response.value.error |: inputs_are_error
    ; finished = finished.value
    ; read_bus = Memory.Read_bus.Tx.Of_always.value read_request
    }
  ;;

  let hierarchical ~instance (scope : Scope.t) (input : Signal.t I.t) =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ~scope ~name:"load" ~instance create input
  ;;
end
