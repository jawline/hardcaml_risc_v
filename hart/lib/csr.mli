open! Core
open Hardcaml

module Make
    (Hart_config : Hart_config_intf.S)
    (Registers : Registers_intf.S)
    (Decoded_instruction : Decoded_instruction_intf.M(Registers).S) : sig
  module I : sig
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; valid : 'a
      ; instruction : 'a Decoded_instruction.t
      ; instret : 'a
      }
    [@@deriving hardcaml]
  end

  module O : sig
    type 'a t =
      { valid : 'a [@rtlprefix "output_"]
      ; value : 'a [@bits register_width]
      }
    [@@deriving hardcaml]
  end

  val hierarchical : Scope.t -> Signal.t I.t -> Signal.t O.t
end
