(** Store implements SW, SH and SB. It implements
    a state machine that first collects the current state of the word around
    and address, then writes the new desired word at the align address to a
    register, then waits for the memory controller to write that word out.

    We do not support unaligned writes. *)
open! Core

open Hardcaml
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
      ; destination : 'a [@bits register_width]
      ; value : 'a [@bits register_width]
      ; memory_controller_to_hart : 'a Memory.Rx_bus.Tx.t
           [@rtlprefix "memory_controller_to_hart$"]
      ; hart_to_memory_controller : 'a Memory.Tx_bus.Rx.t
           [@rtlprefix "hart_to_memory_controller$"]
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t =
      { new_rd : 'a [@bits register_width] [@rtlname "new_rd"]
      ; error : 'a
      ; finished : 'a
      ; memory_controller_to_hart : 'a Memory.Rx_bus.Rx.t
           [@rtlprefix "memory_controller_to_hart$"]
      ; hart_to_memory_controller : 'a Memory.Tx_bus.Tx.t
           [@rtlprefix "hart_to_memory_controller$"]
      }
    [@@deriving sexp_of, hardcaml]
  end

  module State = struct
    type t =
      | Preparing_load
      | Waiting_for_load
      | Preparing_store
      | Waiting_for_store
    [@@deriving sexp, enumerate, compare]
  end

  let create
    (scope : Scope.t)
    ({ I.clock
     ; clear
     ; enable
     ; funct3
     ; destination
     ; value
     ; memory_controller_to_hart
     ; hart_to_memory_controller = { ready = memory_controller_ready }
     } :
      _ I.t)
    =
    (* TODO: There is an awful lot of overlap with load. Particularly
       re alignment. Share the logic. *)
    let ( -- ) = Scope.naming scope in
    let reg_spec = Reg_spec.create ~clock ~clear () in
    let current_state = State_machine.create (module State) reg_spec in
    ignore (current_state.current -- "current_state" : Signal.t);
    let hart_to_memory_controller = Memory.Tx_bus.Tx.Of_always.wire zero in
    let aligned_address =
      (* Mask the read address to a 4-byte alignment. *)
      destination &: ~:(of_int ~width:register_width 0b11) -- "aligned_address"
    in
    let unaligned_bits =
      Util.switch
        (module Funct3.Load)
        ~if_not_found:(zero 2)
        ~f:(function
          | Funct3.Load.Lw -> uresize destination 2 &:. 0b11
          | Lh | Lhu -> uresize destination 2 &:. 0b1
          | Lb | Lbu -> zero 2)
        funct3
      -- "unaligned_bits"
    in
    let is_load_word = Util.is (module Funct3.Load) funct3 Funct3.Load.Lw in
    let is_unaligned = (unaligned_bits <>:. 0) -- "is_unaligned" in
    let funct3_is_error =
      Util.switch (module Funct3.Load) ~if_not_found:(one 1) ~f:(fun _ -> zero 1) funct3
      -- "funct3_is_error"
    in
    let inputs_are_error = is_unaligned |: funct3_is_error -- "inputs_are_error" in
    let word_to_write =
      (* The word to write back to memory during
         Waiting_for_store.  If we are writing a full word, this is set on cycle 0,
         otherwise we need to do a load from the memory controller and concat it
         with the desire write to produce the word to write back. *)
      Variable.reg ~width:register_width reg_spec
    in
    compile
      [ current_state.switch
          [ ( State.Preparing_load
            , [ when_
                  (enable &: ~:inputs_are_error)
                  [ (* If we are loading a whole word and it is aligned
                       we do not need to read from the memory controller,
                       we can just write the whole thing skipping the
                       load step.
                    *)
                    if_
                      is_load_word
                      [ word_to_write <-- value
                      ; current_state.set_next Waiting_for_store
                      ]
                      [ Memory.Tx_bus.Tx.Of_always.assign
                          hart_to_memory_controller
                          { valid = one 1
                          ; data =
                              { address = aligned_address
                              ; write = zero 1
                              ; write_data = zero 32
                              }
                          }
                      ; when_
                          memory_controller_ready
                          [ current_state.set_next Waiting_for_load ]
                      ]
                  ]
              ] )
          ; ( Waiting_for_load
            , [ when_
                  memory_controller_to_hart.valid
                  [ word_to_write
                    <-- (* TODO: This should mux out the changed word from the loaded word and then or it with new one shifted by the alignment. *)
                    assert false
                  ; current_state.set_next Waiting_for_store
                  ]
              ] )
          ; ( Preparing_store
            , [ Memory.Tx_bus.Tx.Of_always.assign
                  hart_to_memory_controller
                  { valid = one 1
                  ; data =
                      { address = aligned_address
                      ; write = one 1
                      ; write_data = word_to_write.value
                      }
                  }
              ; when_ memory_controller_ready [ current_state.set_next Waiting_for_store ]
              ] )
          ]
      ; ( Waiting_for_load
        , [ when_
              memory_controller_to_hart.valid
              [ current_state.set_next Preparing_load ]
          ] )
      ];
    { O.new_rd = assert false
    ; error = memory_controller_to_hart.data.error |: inputs_are_error
    ; finished =
        (* We are finished either once the memory controller responds with a write finished signal OR immediately with error if we are unaligned. *)
        is_unaligned
        |: (memory_controller_to_hart.valid &: current_state.is Preparing_store)
    ; memory_controller_to_hart = { Memory.Rx_bus.Rx.ready = one 1 }
    ; hart_to_memory_controller =
        Memory.Tx_bus.Tx.Of_always.value hart_to_memory_controller
    }
  ;;

  let hierarchical ~instance (scope : Scope.t) (input : Signal.t I.t) =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ~scope ~name:"Store" ~instance create input
  ;;
end
