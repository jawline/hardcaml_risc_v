(** Store implements SW, SH and SB. It implements
    a state machine that first collects the current state of the word around
    and address, then writes the new desired word at the align address to a
    register, then waits for the memory controller to write that word out.

    We do not support unaligned writes. *)
open! Core

open Hardcaml
open Hardcaml_memory_controller
open! Signal
open Always

(* TODO: Move the actual memory write to the write back step in the pipeline so we can terminate it. *)
(* TODO: Memory controller could take a byte enable when writing and manage any
   read before write behaviour itself. *)

module Make (Hart_config : Hart_config_intf.S) (Memory : Memory_bus_intf.S) = struct
  let register_width = Register_width.bits Hart_config.register_width

  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; enable : 'a
      ; funct3 : 'a [@bits 3]
      ; destination : 'a [@bits register_width]
      ; value : 'a [@bits register_width]
      ; write_bus : 'a Memory.Write_bus.Dest.t
      ; read_bus : 'a Memory.Read_bus.Dest.t
      ; write_response : 'a Memory.Write_response.With_valid.t
      ; read_response : 'a Memory.Read_response.With_valid.t
      }
    [@@deriving hardcaml ~rtlmangle:"$"]
  end

  module O = struct
    type 'a t =
      { error : 'a
      ; finished : 'a
      ; write_bus : 'a Memory.Write_bus.Source.t
      ; read_bus : 'a Memory.Read_bus.Source.t
      }
    [@@deriving hardcaml ~rtlmangle:"$"]
  end

  module State = struct
    type t =
      | Idle
      | Preparing_load
      | Waiting_for_load
      | Preparing_store
      | Waiting_for_store
    [@@deriving sexp, enumerate, compare]
  end

  let combine_old_and_new_word ~funct3 ~destination ~old_word ~new_word _scope =
    mux_init
      ~f:(fun alignment ->
        Util.switch
          (module Funct3.Store)
          ~if_not_found:
            ((* In practice, this arm should be impossible *) zero register_width)
          ~f:(function
            | Funct3.Store.Sw ->
              (* In practice, this isn't possible as we do not do the load step
                 with aligned words and do not support unaligned words. *)
              zero register_width
            | Sh ->
              (* We do not support stores for half words that
                 aren't aligned against the memory bus. The error flag would
                 have already been set so we would never use these signals
                 anyway. *)
              if alignment % 2 <> 0
              then zero register_width
              else (
                let alignment = alignment / 2 in
                let write_word = sel_bottom ~width:16 new_word in
                let parts = split_lsb ~part_width:16 old_word in
                concat_lsb
                  (List.take parts alignment
                   @ [ write_word ]
                   @ List.drop parts (alignment + 1)))
            | Sb ->
              (* We don't have any alignment requirements for byte writes. *)
              let byte = sel_bottom ~width:8 new_word in
              let parts = split_lsb ~part_width:8 old_word in
              concat_lsb
                (List.take parts alignment @ [ byte ] @ List.drop parts (alignment + 1)))
          funct3)
      (uresize ~width:(Int.floor_log2 (register_width / 8)) destination)
      (register_width / 8)
  ;;

  let create
        (scope : Scope.t)
        ({ I.clock
         ; clear
         ; enable
         ; funct3
         ; destination
         ; value
         ; write_bus
         ; read_bus
         ; write_response
         ; read_response
         } :
          _ I.t)
    =
    (* TODO: There is an awful lot of overlap with load. Particularly
       re alignment. Share the logic. *)
    let ( -- ) = Scope.naming scope in
    let reg_spec = Reg_spec.create ~clock ~clear () in
    let reg_spec_no_clear = Reg_spec.create ~clock () in
    let current_state = State_machine.create (module State) reg_spec in
    ignore (current_state.current -- "current_state" : Signal.t);
    let aligned_address =
      (* Mask the read address to a 4-byte alignment. *)
      destination &: ~:(of_unsigned_int ~width:register_width 0b11)
    in
    let unaligned_bits =
      Util.switch
        (module Funct3.Store)
        ~if_not_found:(zero 2)
        ~f:(function
          | Funct3.Store.Sw -> uresize ~width:2 destination &:. 0b11
          | Sh -> uresize ~width:2 destination &:. 0b1
          | Sb -> zero 2)
        funct3
      -- "unaligned_bits"
    in
    let is_load_word = Util.is (module Funct3.Store) funct3 Funct3.Store.Sw in
    let is_unaligned = unaligned_bits <>:. 0 in
    let funct3_is_error =
      Util.switch (module Funct3.Store) ~if_not_found:vdd ~f:(fun _ -> gnd) funct3
    in
    let inputs_are_error = is_unaligned |: funct3_is_error in
    let word_to_write =
      (* The word to write back to memory during
         Waiting_for_store.  If we are writing a full word, this is set on cycle 0,
         otherwise we need to do a load from the memory controller and concat it
         with the desire write to produce the word to write back. *)
      Variable.reg ~width:register_width reg_spec_no_clear
    in
    let store_finished = Variable.wire ~default:gnd in
    (* TODO: Error signal is not correctly propagated here. *)
    compile
      [ current_state.switch
          [ ( State.Idle
            , [ when_
                  enable
                  [ if_
                      inputs_are_error
                      [ store_finished <-- vdd ]
                      [ if_
                          is_load_word
                          [ word_to_write <-- value
                          ; current_state.set_next Preparing_store
                          ]
                          [ (* To ease timings, we don't issue a load thsi cycle. *)
                            current_state.set_next Preparing_load
                          ]
                      ]
                  ]
              ] )
          ; ( Preparing_load
            , [ when_ read_bus.ready [ current_state.set_next Waiting_for_load ] ] )
          ; ( Waiting_for_load
            , [ when_
                  read_response.valid
                  [ word_to_write
                    <-- combine_old_and_new_word
                        (* Here we supply the
                           unaligned destination as it is used to decide how to
                           rewrite the word. *)
                          ~funct3
                          ~destination:
                            (reg ~enable:(current_state.is Idle) reg_spec destination)
                          ~old_word:read_response.value.read_data
                          ~new_word:value
                          scope
                  ; current_state.set_next Preparing_store
                  ]
              ] )
          ; ( Preparing_store
            , [ when_ write_bus.ready [ current_state.set_next Waiting_for_store ] ] )
          ; ( Waiting_for_store
            , [ when_
                  write_response.valid
                  [ store_finished <--. 1; current_state.set_next Idle ]
              ] )
          ]
      ];
    { O.error = inputs_are_error (* TODO: Write errors are getting ignored here. *)
    ; finished =
        (* We are finished either once the memory controller responds with a
           write finished signal OR immediately with error if we are unaligned.
        *)
        store_finished.value
    ; write_bus =
        { valid = current_state.is Preparing_store
        ; data =
            { address = reg ~enable:(current_state.is Idle) reg_spec aligned_address
            ; write_data = word_to_write.value
            }
        }
    ; read_bus =
        { valid = current_state.is Preparing_load
        ; data =
            { address = reg ~enable:(current_state.is Idle) reg_spec aligned_address }
        }
    }
  ;;

  let hierarchical ~instance (scope : Scope.t) (input : Signal.t I.t) =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ~scope ~name:"store" ~instance create input
  ;;
end
