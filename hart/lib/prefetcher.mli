(* TODO: Pre load a few memory addresses rather than the next memory address. *)
(** 
 The prefetcher module helps the fetcher by pre-loading the next instruction into
 memory. On valid, the prefetcher will schedule a memory read and then immediately
 schedule a read to the address the next instruction would be if the code does not
 branch.

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
      ; board_clear : 'a
      ; hart_clear : 'a
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
