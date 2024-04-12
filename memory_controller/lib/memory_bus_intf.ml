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

  module Tx_bus : Stream_intf.S with type 'a data := 'a Tx_data.t
  module Rx_bus : Stream_intf.S with type 'a data := 'a Rx_data.t
end
