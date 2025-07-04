(** 
 The prefetcher module allows us to schedule and cache a single memory read.
 This can be used by fetch to speculatively fetch the next instruction. 

 It keeps an address fifo of the reads that are in flight and issues reads
 whenever valid is set. It will then emit the value and address when the
 read is complete, latching on the state of the last read that completed. We
 can use these to speculatively issue reads, but override the issue if it is
 wrong. *)
open! Core

open Hardcaml
open Hardcaml_memory_controller

module Make (Memory : Memory_bus_intf.S) : sig
  module I : sig
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; valid : 'a [@rtlprefix "input_"]
      ; aligned_address : 'a [@bits address_width]
      ; read_bus : 'a Memory.Read_bus.Dest.t
      ; read_response : 'a Memory.Read_response.With_valid.t
      }
    [@@deriving hardcaml]
  end

  module O : sig
    type 'a t =
      { valid : 'a
      ; aligned_address : 'a [@bits address_width]
      ; value : 'a [@bits data_width]
      ; read_bus : 'a Memory.Read_bus.Source.t
      ; ready : 'a
      }
    [@@deriving hardcaml]
  end

  val hierarchical : Scope.t -> Signal.t I.t -> Signal.t O.t
end
