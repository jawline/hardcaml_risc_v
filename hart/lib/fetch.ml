open! Core
open Hardcaml
open Hardcaml_memory_controller
open Signal

module Make
    (Hart_config : Hart_config_intf.S)
    (Memory : Memory_bus_intf.S)
    (Registers : Registers_intf.S) =
struct
  module Prefetcher = Prefetcher.Make (Memory)

  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; valid : 'a [@rtlprefix "input_"]
      ; registers : 'a Registers.For_writeback.t [@rtlprefix "input_"]
      ; read_bus : 'a Memory.Read_bus.Dest.t
      ; read_response : 'a Memory.Read_response.With_valid.t
      }
    [@@deriving hardcaml ~rtlmangle:"$"]
  end

  module O = struct
    type 'a t =
      { valid : 'a
      ; registers : 'a Registers.For_writeback.t
      ; instruction : 'a [@bits 32]
      ; read_bus : 'a Memory.Read_bus.Source.t
      }
    [@@deriving hardcaml ~rtlmangle:"$"]
  end

  let cut_through_latch ~enable reg_spec signal =
    mux2 enable signal (reg ~enable reg_spec signal)
  ;;

  let create scope ({ clock; clear; valid; registers; read_bus; read_response } : _ I.t) =
    let reg_spec_with_clear = Reg_spec.create ~clock ~clear () in
    let%hw address = registers.pc in
    let%hw prefetcher_ready = wire 1 in
    let%hw want_to_issue_fetch =
      valid |: reg_fb ~width:1 ~f:(fun t -> t &: ~:prefetcher_ready) reg_spec_with_clear
    in
    let aligned_address =
      cut_through_latch
        ~enable:valid
        reg_spec_with_clear
        (Memory.byte_address_to_memory_address address).value
    in
    let prefetcher =
      Prefetcher.hierarchical
        scope
        { clock
        ; clear
        ; valid = want_to_issue_fetch
        ; aligned_address
        ; read_bus
        ; read_response
        }
    in
    prefetcher_ready <-- prefetcher.ready;
    (* TODO: These can optionally be made cut through for zero cycles of
       latency but a longer path. *)
    let registers =
      Registers.For_writeback.Of_signal.reg ~enable:valid reg_spec_with_clear registers
    in
    let output_valid =  prefetcher.valid &: (prefetcher.aligned_address ==: aligned_address) in
    (* Was valid guards that we only raise valid once per input valid. This gets set when we output valid and reset
       when we see valid as an input. *)
    let was_valid = reg_fb ~width:1 ~f:(fun t -> 
        ~:(valid) &: (t |: output_valid)) reg_spec_with_clear in
    { O.read_bus = prefetcher.read_bus
    ; valid = output_valid &: ~:(was_valid)
    ; registers
    ; instruction = prefetcher.value
    }
  ;;

  let hierarchical (scope : Scope.t) (input : Signal.t I.t) =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ~scope ~name:"fetch" create input
  ;;
end
