open! Core
open Hardcaml

module Make
    (Memory_bus : Memory_bus_intf.S)
    (M : sig
       val capacity_in_bytes : int
       val num_read_channels : int
       val num_write_channels : int
       val data_bus_width : int
     end) : sig
  module I : sig
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; which_read_ch : 'a
      ; selected_read_ch : 'a Memory_bus.Read_bus.Source.t
      ; which_write_ch : 'a
      ; selected_write_ch : 'a Memory_bus.Write_bus.Source.t
      }
    [@@deriving hardcaml]
  end

  module O : sig
    type 'a t =
      { read_response : 'a Memory_bus.Read_response.With_valid.t list
      ; write_response : 'a Memory_bus.Write_response.With_valid.t list
      }
    [@@deriving hardcaml]
  end

  val hierarchical : read_latency:int -> Scope.t -> Signal.t I.t -> Signal.t O.t
end
