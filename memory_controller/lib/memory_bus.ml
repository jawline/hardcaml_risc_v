open Core
open Hardcaml
open Hardcaml_stream

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

  let address_is_word_aligned address =
    let unaligned_bits = Int.floor_log2 (M.data_bus_width / 8) in
    Signal.(address &:. unaligned_bits)
  ;;

  module Tx_bus = Stream.Make (Tx_data)
  module Rx_bus = Stream.Make (Rx_data)
end
