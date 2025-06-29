open! Core
open Hardcaml
open Hardcaml_memory_controller
open Signal
open Always

module Make (Hart_config : Hart_config_intf.S) (Memory : Memory_bus_intf.S) = struct
  let data_width = Memory.data_bus_width
  let register_width = Register_width.bits Hart_config.register_width

  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; enable : 'a
      ; op : 'a Funct3.Load.Onehot.t
      ; address : 'a [@bits register_width]
      ; read_bus : 'a Memory.Read_bus.Dest.t
      ; read_response : 'a Memory.Read_response.With_valid.t
      }
    [@@deriving hardcaml ~rtlmangle:"$"]
  end

  module O = struct
    type 'a t =
      { new_rd : 'a [@bits register_width]
      ; error : 'a
      ; finished : 'a
      ; read_bus : 'a Memory.Read_bus.Source.t
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
        ({ I.clock; clear; enable; op; address; read_bus; read_response } : _ I.t)
    =
    let reg_spec = Reg_spec.create ~clock ~clear () in
    let reg_spec_no_clear = Reg_spec.create ~clock () in
    let%hw.State_machine current_state = State_machine.create (module State) reg_spec in
    let%hw_var load_valid = Variable.wire ~default:gnd in
    let%hw aligned_address =
      reg
        ~enable:(current_state.is Idle)
        reg_spec_no_clear
        (Memory.byte_address_to_memory_address address).value
    in
    let%hw is_unaligned =
      Funct3.Load.Onehot.switch
        ~f:(function
          | Funct3.Load.Lw -> sel_bottom ~width:2 address <>:. 0
          | Lh | Lhu -> sel_bottom ~width:1 address
          | Lb | Lbu -> gnd)
        op
    in
    let inputs_are_error = is_unaligned in
    let issue_load =
      proc
        [ load_valid <-- vdd
        ; when_ read_bus.ready [ current_state.set_next Waiting_for_load ]
        ]
    in
    let finished = Variable.wire ~default:gnd in
    let slot_address ~width ~data_width scope =
      let slots = data_width / width in
      let bytes_per_slot = width / 8 in
      let%hw slot_address =
        (* address_bits_for has a min value of one. *)
        if bytes_per_slot = 1
        then address
        else drop_bottom ~width:(address_bits_for (bytes_per_slot - 1)) address
      in
      let%hw slot_address_trunc =
        sel_bottom ~width:(address_bits_for slots) slot_address
      in
      slots, slot_address_trunc
    in
    let how_many_bytes_to_shift ~width ~data_width scope =
      let slots, slot_address = slot_address ~width ~data_width scope in
      mux_init
        ~f:(fun t ->
          of_unsigned_int ~width:(address_bits_for (data_width / 8)) (t * width / 8))
        slot_address
        slots
    in
    let%hw shift_amt_in_bytes =
      reg
        ~enable:(current_state.is Idle)
        reg_spec_no_clear
        (Funct3.Load.Onehot.switch
           ~f:(function
             | Funct3.Load.Lw ->
               how_many_bytes_to_shift
                 ~width:32
                 ~data_width
                 (Scope.sub_scope scope "shift_lw")
             | Lh | Lhu ->
               how_many_bytes_to_shift
                 ~width:16
                 ~data_width
                 (Scope.sub_scope scope "shift_lh")
             | Lb | Lbu ->
               how_many_bytes_to_shift
                 ~width:8
                 ~data_width
                 (Scope.sub_scope scope "shift_lb"))
           op)
    in
    let cached_op =
      Funct3.Load.Onehot.Of_signal.reg
        ~enable:(current_state.is Idle)
        reg_spec_no_clear
        op
    in
    compile
      [ current_state.switch
          [ ( State.Idle
            , [ when_
                  enable
                  [ if_
                      inputs_are_error
                      [ finished <-- vdd ]
                      [ current_state.set_next Waiting_for_memory_controller ]
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
        (let%hw shifted_result =
           log_shift
             ~by:shift_amt_in_bytes
             ~f:(fun t ~by -> srl ~by:(by * 8) t)
             read_response.value.read_data
         in
         let%hw full_word = sel_bottom ~width:register_width shifted_result in
         let%hw half_word = sel_bottom ~width:16 shifted_result in
         let%hw byte = sel_bottom ~width:8 shifted_result in
         Funct3.Load.Onehot.switch
           ~f:(function
             | Funct3.Load.Lw -> full_word
             | Lh -> Util.sign_extend ~width:register_width half_word
             | Lhu -> uresize ~width:register_width half_word
             | Lb -> Util.sign_extend ~width:register_width byte
             | Lbu -> uresize ~width:register_width byte)
           cached_op)
    ; error = read_response.valid |: inputs_are_error
    ; finished = finished.value
    ; read_bus = { valid = load_valid.value; data = { address = aligned_address } }
    }
  ;;

  let hierarchical (scope : Scope.t) (input : Signal.t I.t) =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ~scope ~name:"load" create input
  ;;
end
