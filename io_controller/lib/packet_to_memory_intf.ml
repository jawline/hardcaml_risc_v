open! Core
open Hardcaml
open Hardcaml_axi
open Hardcaml_memory_controller

module M (Memory : Memory_bus_intf.S) (Axi : Stream.S) = struct
  module type S = sig
    module I : sig
      type 'a t =
        { clock : 'a Clocking.t
        ; in_ : 'a Axi.Source.t
        ; out : 'a Memory.Write_bus.Dest.t
        ; out_ack : 'a Memory.Write_response.With_valid.t
        }
      [@@deriving hardcaml]
    end

    module O : sig
      type 'a t =
        { in_ : 'a Axi.Dest.t
        ; out : 'a Memory.Write_bus.Source.t
        }
      [@@deriving hardcaml]
    end

    val hierarchical : Scope.t -> Signal.t I.t -> Signal.t O.t
  end
end
