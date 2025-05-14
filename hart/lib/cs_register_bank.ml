open! Core
open Hardcaml
open Signal

(* TODO: Test *)

module type Bank_config = sig
  val address_bits : int
  val register_io : valid:t -> address:t -> also_write:t -> value:t -> t With_valid.t
end

(** This forms a generic bank of registers we can use to implement the CS
    (control & status) registers. These expose information about the system
    environment to the running software (e.g., clock cycles, time since
    startup). The CS registers are formed out of a set of banks which have
    contiguous register allocations. Register address are register width
    aligned. *)
module Make (Hart_config : Hart_config_intf.S) (Bank_config : Bank_config) = struct
  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; enable : 'a
      (** Enable should be pulsed rather than held high. Each pulse indicates a unique read / write. The caller should expect a response, but it may take many cycles. *)
      ; is_write : 'a
      ; write_value : 'a [@bits Register_width.bits Hart_config.register_width]
      ; address : 'a [@bits Bank_config.address_bits] [@rtlname "i_address"]
      }
    [@@deriving hardcaml ~rtlmangle:"i$"]
  end

  module O = struct
    type 'a t =
      { valid : 'a
      ; value : 'a [@bits Register_width.bits Hart_config.register_width]
      }
    [@@deriving hardcaml ~rtlmangle:"o$"]
  end

  let create _scope { I.clock = _; clear = _; enable; is_write; address; write_value } =
    let result =
      Bank_config.register_io
        ~valid:enable
        ~address
        ~also_write:is_write
        ~value:write_value
    in
    { O.valid = result.valid; value = result.value }
  ;;

  let hierarchical (scope : Scope.t) (input : Signal.t I.t) =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ~scope ~name:"cs_register_bank" create input
  ;;
end
