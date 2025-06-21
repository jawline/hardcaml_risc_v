(** The instruction executor. Takes in a decoded opcode and a register table
    and produces a Transaction expressing the result of this operation. The
    transaction is used to update the processor state during the final write
    back step. *)
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
      ; ecall_transaction : 'a Transaction.With_valid.t
      ; write_bus : 'a Memory.Write_bus.Dest.t
      ; read_bus : 'a Memory.Read_bus.Dest.t
      ; write_response : 'a Memory.Write_response.With_valid.t
      ; read_response : 'a Memory.Read_response.With_valid.t
      ; instret : 'a
      }
    [@@deriving hardcaml]
  end

  module O : sig
    type 'a t =
      { valid : 'a
      ; registers : 'a Registers.For_writeback.t
      ; instruction : 'a Decoded_instruction.t
      ; transaction : 'a Transaction.t
      ; is_ecall : 'a
      ; write_bus : 'a Memory.Write_bus.Source.t
      ; read_bus : 'a Memory.Read_bus.Source.t
      ; error : 'a
      }
    [@@deriving hardcaml]
  end

  val hierarchical : Scope.t -> Signal.t I.t -> Signal.t O.t
end
