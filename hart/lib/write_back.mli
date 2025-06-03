open! Core
open Hardcaml
open Hardcaml_memory_controller

module Make
    (Hart_config : Hart_config_intf.S)
    (Memory : Memory_bus_intf.S)
    (Registers : Registers_intf.S)
    (Decoded_instruction : Decoded_instruction_intf.M(Registers).S)
    (Transaction : Transaction_intf.S) : sig
  module I : sig
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; valid : 'a
      ; registers : 'a Registers.For_writeback.t
      ; instruction : 'a Decoded_instruction.t
      ; transaction : 'a Transaction.t
      ; error : 'a
      }
    [@@deriving hardcaml]
  end

  module O : sig
    type 'a t =
      { valid : 'a
      ; registers : 'a Registers.For_writeback.t
      ; error : 'a
      }
    [@@deriving hardcaml]
  end

  val hierarchical : Scope.t -> Signal.t I.t -> Signal.t O.t
end
