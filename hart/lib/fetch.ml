open! Core
open Hardcaml
open Hardcaml_memory_controller
open Signal

module Make
    (Hart_config : Hart_config_intf.S)
    (Memory : Memory_bus_intf.S)
    (Registers : Registers_intf.S) =
struct
  let register_width = Register_width.bits Hart_config.register_width

  module Prefetcher = Prefetcher.Make (Memory)

  module I = struct
    type 'a t =
      { clock : 'a Clocking.t
      ; valid : 'a [@rtlprefix "input_"]
      ; pc : 'a [@bits register_width]
      ; read_bus : 'a Memory.Read_bus.Dest.t
      ; read_response : 'a Memory.Read_response.With_valid.t
      }
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t =
      { valid : 'a
      ; instruction : 'a [@bits 32]
      ; read_bus : 'a Memory.Read_bus.Source.t
      }
    [@@deriving hardcaml]
  end

  module Without_registers = struct
    let create scope ({ clock; valid; pc; read_bus; read_response } : _ I.t) =
      let reg_spec_with_clear = Clocking.to_spec clock in
      let%hw address = pc in
      let%hw prefetcher_ready = wire 1 in
      let%hw want_to_issue_fetch =
        valid
        |: reg_fb
             ~width:1
             ~f:(fun t -> mux2 prefetcher_ready gnd (t |: valid))
             reg_spec_with_clear
      in
      let%hw aligned_address =
        cut_through_reg
          ~enable:valid
          reg_spec_with_clear
          (Memory.byte_address_to_memory_address address).value
      in
      let prefetcher =
        Prefetcher.hierarchical
          scope
          { clock; valid = want_to_issue_fetch; aligned_address; read_bus; read_response }
      in
      prefetcher_ready <-- prefetcher.ready;
      let output_valid = prefetcher.valid in
      (* Was valid guards that we only raise valid once per input valid. This gets set when we output valid and reset
       when we see valid as an input. *)
      let%hw was_valid =
        Clocking.reg_fb
          ~width:1
          ~clear_to:vdd
          ~enable:(valid |: output_valid)
          ~f:(fun t -> mux2 valid ~:output_valid (t &: ~:output_valid))
          clock
      in
      { O.read_bus = prefetcher.read_bus
      ; valid = output_valid &: (valid |: was_valid)
      ; instruction = prefetcher.value
      }
    ;;

    let hierarchical (scope : Scope.t) (input : Signal.t I.t) =
      let module H = Hierarchy.In_scope (I) (O) in
      H.hierarchical ~scope ~name:"fetch_inner" create input
    ;;
  end

  let create scope (i : _ I.t) =
    let reg_spec_with_clear = Clocking.to_spec i.clock in
    let reg_spec_no_clear = Clocking.to_spec_no_clear i.clock in
    let o = Without_registers.hierarchical scope i in
    let maybe_reg ~enable spec =
      if Hart_config.register_fetch_output then reg ~enable spec else Fn.id
    in
    { O.read_bus = o.read_bus (* Never registered *)
    ; valid = maybe_reg ~enable:vdd reg_spec_with_clear o.valid
    ; instruction = maybe_reg ~enable:o.valid reg_spec_no_clear o.instruction
    }
  ;;

  let hierarchical (scope : Scope.t) (input : Signal.t I.t) =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ~scope ~name:"fetch" create input
  ;;
end
