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
      ; source : 'a [@bits register_width]
      ; memory_controller_to_hart : 'a Memory.Rx_bus.Tx.t
           [@rtlprefix "memory_controller_to_hart"]
      ; hart_to_memory_controller : 'a Memory.Tx_bus.Rx.t
           [@rtlprefix "hart_to_memory_controller"]
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t =
      { new_rd : 'a [@bits register_width] [@rtlname "new_rd"]
      ; error : 'a
      ; finished : 'a
      ; memory_controller_to_hart : 'a Memory.Rx_bus.Rx.t
           [@rtlprefix "memory_controller_to_hart"]
      ; hart_to_memory_controller : 'a Memory.Tx_bus.Tx.t
           [@rtlprefix "hart_to_memory_controller"]
      }
    [@@deriving sexp_of, hardcaml]
  end

  module State = struct
    type t =
      | Waiting_for_memory_controller
      | Waiting_for_load
    [@@deriving sexp, enumerate, compare]
  end

  let create
    (scope : Scope.t)
    ({ I.clock
     ; clear
     ; enable
     ; funct3
     ; source
     ; memory_controller_to_hart
     ; hart_to_memory_controller = { ready = memory_controller_ready }
     } :
      _ I.t)
    =
    (* TODO: We currently disallow loads that are not aligned on a {load width}
       boundary. We could support this by loading a second word and muxing the
       result at the cost of an extra load cycle.  *)
    let ( -- ) = Scope.naming scope in
    let reg_spec = Reg_spec.create ~clock ~clear () in
    let current_state = State_machine.create (module State) reg_spec in
    ignore (current_state.current -- "current_state" : Signal.t);
    let hart_to_memory_controller = Memory.Tx_bus.Tx.Of_always.wire zero in
    let aligned_address =
      (* Mask the read address to a 4-byte alignment. *)
      source &: ~:(of_int ~width:register_width 0b11) -- "aligned_address"
    in
    let unaligned_bits =
      Util.switch
        (module Funct3.Load)
        ~if_not_found:(zero 2)
        ~f:(function
          | Funct3.Load.Lw -> uresize source 2 &:. 0b11
          | Lh | Lhu -> uresize source 2 &:. 0b1
          | Lb | Lbu -> zero 2)
        funct3
      -- "unaligned_bits"
    in
    let is_unaligned = (unaligned_bits <>:. 0) -- "is_unaligned" in
    let funct3_is_error =
      Util.switch (module Funct3.Load) ~if_not_found:(one 1) ~f:(fun _ -> zero 1) funct3
      -- "funct3_is_error"
    in
    let inputs_are_error = is_unaligned |: funct3_is_error -- "inputs_are_error" in
    compile
      [ current_state.switch
          [ ( State.Waiting_for_memory_controller
            , [ when_
                  (enable &: memory_controller_ready &: ~:inputs_are_error)
                  [ Memory.Tx_bus.Tx.Of_always.assign
                      hart_to_memory_controller
                      { valid = one 1
                      ; data =
                          { address = aligned_address
                          ; write = zero 1
                          ; write_data = zero 32
                          }
                      }
                  ; current_state.set_next Waiting_for_load
                  ]
              ] )
          ; ( Waiting_for_load
            , [ when_
                  memory_controller_to_hart.valid
                  [ current_state.set_next Waiting_for_memory_controller ]
              ] )
          ]
      ];
    { O.new_rd =
        (let alignment_bits = source &:. 0b11 in
         let full_word = memory_controller_to_hart.data.read_data in
         let half_word =
           mux2
             (alignment_bits ==:. 0)
             (sel_top full_word (register_width / 2))
             (sel_bottom full_word (register_width / 2))
         in
         let byte = mux alignment_bits (split_msb ~part_width:8 full_word) in
         Util.switch
           (module Funct3.Load)
           ~if_not_found:(zero register_width)
           ~f:(function
             | Funct3.Load.Lw -> memory_controller_to_hart.data.read_data
             | Lh -> Decoder.sign_extend ~width:register_width half_word
             | Lhu -> uresize half_word register_width
             | Lb -> Decoder.sign_extend ~width:register_width byte
             | Lbu -> uresize byte register_width)
           funct3)
    ; error = memory_controller_to_hart.data.error |: inputs_are_error
    ; finished = is_unaligned |: memory_controller_to_hart.valid
    ; memory_controller_to_hart = { Memory.Rx_bus.Rx.ready = one 1 }
    ; hart_to_memory_controller =
        Memory.Tx_bus.Tx.Of_always.value hart_to_memory_controller
    }
  ;;

  let hierarchical ~instance (scope : Scope.t) (input : Signal.t I.t) =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ~scope ~name:"Load" ~instance create input
  ;;
end
