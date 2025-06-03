(** This forms the control & status register set. The control & status registers
    are a memory region, addressed in register widths, which software can
    use to read from the device state or change device state.

    The registers are split into small contiguous banks, each having a specific
    purpose. The banks are not necessarily contiguous. To implement this
    we activate a bank based on the upper bits of an address and then mux the result
    based on the same upper bits. The lower bits are used as the bank address.*)
open! Core

open Hardcaml

(* TODO: Test *)

module Make (Hart_config : Hart_config_intf.S) : sig
  module I : sig
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; enable : 'a
      ; is_write : 'a
      ; address : 'a [@bits 12]
      ; write_value : 'a [@bits Register_width.bits Hart_config.register_width]
      ; instret : 'a
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

  val hierarchical : clock_frequency:int -> Scope.t -> Signal.t I.t -> Signal.t O.t
end
