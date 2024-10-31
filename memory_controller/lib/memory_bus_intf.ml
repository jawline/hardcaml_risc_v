open Hardcaml
open Hardcaml_stream

module type S = sig
  module Read : sig
    type 'a t = { address : 'a [@bits M.address_width] } [@@deriving hardcaml]
  end

  module Write : sig
    type 'a t =
      { address : 'a [@bits M.address_width]
      ; write_data : 'a [@bits M.data_bus_width]
      }
    [@@deriving hardcaml]
  end

  module Read_response : sig
    type 'a t =
      { error : 'a
      ; read_data : 'a [@bits M.data_bus_width]
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

  module Read_bus : Stream_intf.S with type 'a data := 'a Read.t
  module Write_bus : Stream_intf.S with type 'a data := 'a Write.t
end
