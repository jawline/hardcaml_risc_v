(** An implementation of a one in flight pipeline for execution of Risc-V
    instructions. 
    
    Upon an input valid signal and a set of registers, this unit well fetch,
    decode, execute an instruction then write-back the result to the register
    file over several cycles. If a second input enters the pipeline before the
    first has been emitted the result is not well defined as this pipeline does
    not currently detect hazards. *)

(* TODO: Relax the one in flight semantic and do hazard detection. *)
open! Core
open Hardcaml
open Hardcaml_memory_controller

val required_read_channels : int
val required_write_channels : int

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
      ; ecall_transaction : 'a Transaction.With_valid.t
      ; instret : 'a
      ; write_bus : 'a Memory.Write_bus.Dest.t list [@length required_write_channels]
      ; write_response : 'a Memory.Write_response.With_valid.t list
            [@length required_write_channels]
      ; read_bus : 'a Memory.Read_bus.Dest.t list [@length required_read_channels]
      ; read_response : 'a Memory.Read_response.With_valid.t list
            [@length required_read_channels]
      }
    [@@deriving hardcaml]
  end

  module O : sig
    type 'a t =
      { valid : 'a
      ; registers : 'a Registers.For_writeback.t
      ; is_ecall : 'a
      ; write_bus : 'a Memory.Write_bus.Source.t list [@length required_write_channels]
      ; read_bus : 'a Memory.Read_bus.Source.t list [@length required_read_channels]
      ; fault : 'a
      }
    [@@deriving hardcaml]
  end

  val hierarchical : Scope.t -> Signal.t I.t -> Signal.t O.t
end
