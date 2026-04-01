(** An implementation of a one in flight Risc-V Hart. On clear the execution
    pipeline is started.  It is restarted with the new state of the registers
    each time the execution pipeline pulses a valid signal with a new set of
    registers. *)
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
      { clock : 'a Clocking.t
      ; ecall_transaction : 'a Transaction.With_valid.t
        (* When is_ecall is high the opcode will be considered finished when
             ecall_transaction is finished. If a user wants custom behaviour on ecall
             they should hold ecall finished low, do the work, then raise finished. *)
      ; read_instruction : 'a Memory.Read_bus.Dest.t
      ; read_instruction_response : 'a Memory.Read_response.With_valid.t
      ; read_data : 'a Memory.Read_bus.Dest.t
      ; read_data_response : 'a Memory.Read_response.With_valid.t
      ; write_data : 'a Memory.Write_bus.Dest.t
      ; write_data_response : 'a Memory.Write_response.With_valid.t
      }
    [@@deriving hardcaml, fields ~getters]
  end

  module O : sig
    type 'a t =
      { registers : 'a Registers.t
      ; is_ecall : 'a
        (** Set high when the hart is in an ecall and is delagating behaviour to
          the user design. *)
      ; read_instruction : 'a Memory.Read_bus.Source.t
      ; read_data : 'a Memory.Read_bus.Source.t
      ; write_data : 'a Memory.Write_bus.Source.t
      }
    [@@deriving hardcaml, fields ~getters]
  end

  val hierarchical : ?instance:string -> Scope.t -> Signal.t I.t -> Signal.t O.t
end
