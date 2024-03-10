module Make (M : sig
    val address_width : int
    val data_bus_width : int
  end) =
struct
  module Tx_data = struct
    type 'a t =
      { address : 'a [@bits M.address_width]
      ; write : 'a
      ; write_data : 'a [@bits M.data_bus_width]
      }
    [@@deriving sexp_of, hardcaml]
  end

  module Rx_data = struct
    type 'a t =
      { error : 'a
      ; read_data : 'a [@bits M.data_bus_width]
      }
    [@@deriving sexp_of, hardcaml]
  end

  module Tx_bus = Bus.Make (Tx_data)
  module Rx_bus = Bus.Make (Rx_data)
end
