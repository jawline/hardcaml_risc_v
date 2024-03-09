module type Memory_bus = sig
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

  module Tx_bus : Bus_intf.S with type data = Tx_Data.t
  module Rx_bus : Bus_intf.S with type data = Rx_Data.t
end
