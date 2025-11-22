(** Store implements SW, SH and SB. It writes using byte enables.

    We do not support unaligned writes. *)
open! Core

open Hardcaml
open Hardcaml_memory_controller
open Signal
open Always

(* TODO: Move the actual memory write to the write back step in the pipeline so we can terminate it. *)

module Make (Hart_config : Hart_config_intf.S) (Memory : Memory_bus_intf.S) = struct
  let data_width = Memory.data_bus_width
  let register_width = Register_width.bits Hart_config.register_width

  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; enable : 'a
      ; op : 'a Funct3.Store.Onehot.t
      ; destination : 'a [@bits register_width]
      ; value : 'a [@bits register_width]
      ; write_bus : 'a Memory.Write_bus.Dest.t
      ; write_response : 'a Memory.Write_response.With_valid.t
      }
    [@@deriving hardcaml ~rtlmangle:"$"]
  end

  module O = struct
    type 'a t =
      { error : 'a
      ; finished : 'a
      ; write_bus : 'a Memory.Write_bus.Source.t
      }
    [@@deriving hardcaml ~rtlmangle:"$"]
  end

  module State = struct
    type t =
      | Idle
      | Preparing_store
      | Waiting_for_store
    [@@deriving sexp, enumerate, compare ~localize]
  end

  let create
        (scope : Scope.t)
        ({ I.clock; clear; enable; op; destination; value; write_bus; write_response } :
          _ I.t)
    =
    (* TODO: There is an awful lot of overlap with load. Particularly
       re alignment. Share the logic. *)
    let ( -- ) = Scope.naming scope in
    let reg_spec = Reg_spec.create ~clock ~clear () in
    let reg_spec_no_clear = Reg_spec.create ~clock () in
    let%hw.State_machine current_state = State_machine.create (module State) reg_spec in
    let%hw aligned_address =
      reg
        ~enable:(current_state.is Idle)
        reg_spec_no_clear
        (Memory.byte_address_to_memory_address destination).value
    in
    let%hw unaligned_bits =
      Funct3.Store.Onehot.switch
        ~f:(function
          | Funct3.Store.Sw -> sel_bottom ~width:2 destination &:. 0b11
          | Sh -> sel_bottom ~width:2 destination &:. 0b1
          | Sb -> zero 2)
        op
      -- "unaligned_bits"
    in
    let is_unaligned = unaligned_bits <>:. 0 in
    let inputs_are_error = is_unaligned in
    let slot_address ~width ~data_width scope =
      let slots = data_width / width in
      let bytes_per_slot = width / 8 in
      let%hw slot_address =
        (* address_bits_for has a min value of one. *)
        if bytes_per_slot = 1
        then destination
        else drop_bottom ~width:(address_bits_for (bytes_per_slot - 1)) destination
      in
      let%hw slot_address_trunc =
        sel_bottom ~width:(address_bits_for slots) slot_address
      in
      slots, slot_address_trunc
    in
    let%hw word_to_write =
      (* The word to write back to memory during
         Waiting_for_store.  If we are writing a full word, this is set on cycle 0,
         otherwise we need to do a load from the memory controller and concat it
         with the desire write to produce the word to write back. *)
      reg
        ~enable:(current_state.is Idle)
        reg_spec_no_clear
        (let build_shift ~width ~data_width scope =
           let bytes_per_slot = width / 8 in
           let slots, slot_address = slot_address ~width ~data_width scope in
           mux_init
             ~f:(fun t -> of_unsigned_int ~width:(data_width / 8) (bytes_per_slot * t))
             slot_address
             slots
         in
         let%hw shift_amount =
           Funct3.Store.Onehot.switch
             ~f:(function
               | Funct3.Store.Sw ->
                 build_shift ~width:32 ~data_width (Scope.sub_scope scope "sw")
               | Sh -> build_shift ~width:16 ~data_width (Scope.sub_scope scope "sh")
               | Sb -> build_shift ~width:8 ~data_width (Scope.sub_scope scope "sb"))
             op
         in
         let%hw data_to_write = uextend ~width:data_width value in
         log_shift
           ~by:shift_amount
           ~f:(fun t ~by:bytes_ -> sll ~by:(bytes_ * 8) t)
           data_to_write)
    in
    let%hw write_mask =
      reg
        ~enable:(current_state.is Idle)
        reg_spec_no_clear
        (let build_mask ~width ~data_width scope =
           let slots, slot_address = slot_address ~width ~data_width scope in
           mux_init
             ~f:(fun t ->
               With_zero_width.(
                 concat_lsb [ zero (width / 8 * t); repeat ~count:(width / 8) (Some vdd) ])
               |> Option.value_exn
               |> uextend ~width:(data_width / 8))
             slot_address
             slots
         in
         Funct3.Store.Onehot.switch
           ~f:(function
             | Funct3.Store.Sw ->
               build_mask ~width:32 ~data_width (Scope.sub_scope scope "mask_sw")
             | Sh -> build_mask ~width:16 ~data_width (Scope.sub_scope scope "mask_sh")
             | Sb -> build_mask ~width:8 ~data_width (Scope.sub_scope scope "mask_sb"))
           op)
    in
    let store_finished = Variable.wire ~default:gnd () in
    (* TODO: Error signal is not correctly propagated here. *)
    compile
      [ current_state.switch
          [ ( State.Idle
            , [ when_
                  enable
                  [ if_
                      inputs_are_error
                      [ store_finished <-- vdd ]
                      [ current_state.set_next Preparing_store ]
                  ]
              ] )
          ; ( Preparing_store
            , [ when_ write_bus.ready [ current_state.set_next Waiting_for_store ] ] )
          ; ( Waiting_for_store
            , [ when_
                  write_response.valid
                  [ store_finished <-- vdd; current_state.set_next Idle ]
              ] )
          ]
      ];
    { O.error = enable &: inputs_are_error
    ; finished =
        (* We are finished either once the memory controller responds with a
           write finished signal OR immediately with error if we are unaligned.
        *)
        store_finished.value
    ; write_bus =
        { valid = current_state.is Preparing_store
        ; data =
            { address = aligned_address; write_data = word_to_write; wstrb = write_mask }
        }
    }
  ;;

  let hierarchical (scope : Scope.t) (input : Signal.t I.t) =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ~scope ~name:"store" create input
  ;;
end
