(* This module describes the read and write bus types we use to communicate
   between the components in the system and the memory arbitrator. The read and
   write requests each have a handshake (a ready signal that pulses when the
   read is acknowledged) and should be held valid until acknowledged. The
   responses are guaranteed to arrive in order but without a fixed latency. *)
open! Core
open Hardcaml
open Hardcaml_custom_handshake

module Make (M : sig
    val address_width : int
    val data_bus_width : int
  end) : sig
  module Read : sig
    type 'a t = { address : 'a } [@@deriving hardcaml]
  end

  module Write : sig
    type 'a t =
      { address : 'a
      ; write_data : 'a
      ; wstrb : 'a
      }
    [@@deriving hardcaml]
  end

  module Read_response : sig
    type 'a t =
      { error : 'a
      ; read_data : 'a
      }
    [@@deriving hardcaml]

    module With_valid : With_valid.Wrap.S with type 'a value := 'a t
  end

  module Write_response : sig
    type 'a t = { error : 'a } [@@deriving hardcaml]

    module With_valid : With_valid.Wrap.S with type 'a value := 'a t
  end

  val data_bus_width : int
  val address_is_word_aligned : Signal.t -> Signal.t

  module Read_bus : Handshake_intf.S with type 'a data = 'a Read.t
  module Write_bus : Handshake_intf.S with type 'a data = 'a Write.t
end
