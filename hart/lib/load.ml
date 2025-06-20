open! Core
open Hardcaml
open Hardcaml_memory_controller
open Signal
open Always

module Make (Hart_config : Hart_config_intf.S) (Memory : Memory_bus_intf.S) = struct
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
    (* TODO: We currently disallow loads that are not aligned on a {load width}
       boundary. We could support this by loading a second word and muxing the
       result at the cost of an extra load cycle.  *)
    (* TODO: Alignment is currently decided by hardcoded bit numbers but it is
       computable from our word size which would make this module generic over
       64 and 32 bit harts. *)
    let ( -- ) = Scope.naming scope in
    let reg_spec = Reg_spec.create ~clock ~clear () in
    let current_state = State_machine.create (module State) reg_spec in
    ignore (current_state.current -- "current_state" : Signal.t);
    let load_valid = Variable.wire ~default:gnd in
    let%hw aligned_address =
      (* Mask the read address to a 4-byte alignment. *)
      concat_msb [ drop_bottom address ~width:2; zero 2 ]
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
        (let%hw alignment_bits = address &:. 0b11 in
         let%hw full_word = read_response.value.read_data in
         let%hw half_word =
           mux (alignment_bits <>:. 0) (split_lsb ~part_width:16 full_word)
         in
         let%hw byte = mux alignment_bits (split_lsb ~part_width:8 full_word) in
         Funct3.Load.Onehot.switch
           ~f:(function
             | Funct3.Load.Lw -> full_word
             | Lh -> Util.sign_extend ~width:register_width half_word
             | Lhu -> uresize ~width:register_width half_word
             | Lb -> Util.sign_extend ~width:register_width byte
             | Lbu -> uresize ~width:register_width byte)
           op)
    ; error = read_response.valid |: inputs_are_error
    ; finished = finished.value
    ; read_bus =
        { valid = load_valid.value; data = { address = reg reg_spec aligned_address } }
    }
  ;;

  let hierarchical (scope : Scope.t) (input : Signal.t I.t) =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ~scope ~name:"load" create input
  ;;
end
