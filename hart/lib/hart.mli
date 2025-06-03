(** An implementation of a one in flight Risc-V Hart. On clear the execution
    pipeline is started.  It is restarted with the new state of the registers
    each time the execution pipeline pulses a valid signal with a new set of
    registers. *)
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
      ; ecall_transaction : 'a Transaction.With_valid.t
        (* When is_ecall is high the opcode will be considered finished when
             ecall_transaction is finished. If a user wants custom behaviour on ecall
             they should hold ecall finished low, do the work, then raise finished. *)
      ; write_bus : 'a Memory.Write_bus.Dest.t list [@length required_write_channels]
      ; write_response : 'a Memory.Write_response.With_valid.t list
            [@length required_write_channels]
      ; read_bus : 'a Memory.Read_bus.Dest.t list [@length required_read_channels]
      ; read_response : 'a Memory.Read_response.With_valid.t list
            [@length required_read_channels]
      }
    [@@deriving hardcaml, fields ~getters]
  end

  module O : sig
    type 'a t =
      { registers : 'a Registers.t
      ; error : 'a
      ; is_ecall : 'a
        (** Set high when the hart is in an ecall and is delagating behaviour to
          the user design. *)
      ; write_bus : 'a Memory.Write_bus.Source.t list [@length required_write_channels]
      ; read_bus : 'a Memory.Read_bus.Source.t list [@length required_read_channels]
      }
    [@@deriving hardcaml, fields ~getters]
  end

  val hierarchical : ?instance:string -> Scope.t -> Signal.t I.t -> Signal.t O.t
end
