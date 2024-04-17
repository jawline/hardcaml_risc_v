open Hardcaml
open Hardcaml_stream

module type S = sig
  module Tx_data : sig
    type 'a t =
      { address : 'a
      ; write : 'a
      ; write_data : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  module Rx_data : sig
    type 'a t =
      { error : 'a
      ; read_data : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  val data_bus_width : int
  val address_is_word_aligned : Signal.t -> Signal.t

  module Tx_bus : Stream_intf.S with type 'a data := 'a Tx_data.t
  module Rx_bus : Stream_intf.S with type 'a data := 'a Rx_data.t
end
