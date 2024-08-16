open! Core
open! Hardcaml
open Hardcaml_stream

module Make (M : sig
    val data_bus_width : int
  end) =
struct
  module Contents = struct
    type 'a t =
      { data : 'a [@bits M.data_bus_width]
      ; last : 'a
      }
    [@@deriving sexp_of, hardcaml ~rtlmangle:"$"]
  end

  module Contents_stream = Stream.Make (Contents)
end
