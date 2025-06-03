(** This forms a generic bank of registers we can use to implement the CS
    (control & status) registers. These expose information about the system
    environment to the running software (e.g., clock cycles, time since
    startup). The CS registers are formed out of a set of banks which have
    contiguous register allocations. Register address are register width
    aligned. *)
open! Core

open Hardcaml
open Signal

module type Bank_config = sig
  val address_bits : int
  val register_io : valid:t -> address:t -> also_write:t -> value:t -> t With_valid.t
end

module Make (Hart_config : Hart_config_intf.S) (Bank_config : Bank_config) : sig
  module I : sig
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; enable : 'a
        (** Enable should be pulsed rather than held high. Each pulse indicates a unique read / write. The caller should expect a response, but it may take many cycles. *)
      ; is_write : 'a
      ; write_value : 'a [@bits Register_width.bits Hart_config.register_width]
      ; address : 'a [@bits Bank_config.address_bits]
      }
    [@@deriving hardcaml]
  end

  module O : sig
    type 'a t =
      { valid : 'a
      ; value : 'a [@bits Register_width.bits Hart_config.register_width]
      }
    [@@deriving hardcaml]
  end

  val create : Scope.t -> Signal.t I.t -> Signal.t O.t
  val hierarchical : Scope.t -> Signal.t I.t -> Signal.t O.t
end
