open! Core
open Hardcaml
open Signal

module type Switchable = sig
  type t

  val of_int : int -> t option
  val to_int : t -> int
  val all : t list
end

let switch (type a) (module M : Switchable with type t = a) ~f ~if_not_found signal =
  let open M in
  mux_init
    ~f:(fun i -> of_int i |> Option.map ~f |> Option.value ~default:if_not_found)
    signal
    (Int.pow 2 (width signal))
;;

let is (type a) (module M : Switchable with type t = a) signal (opt : M.t) =
  mux2 (signal ==:. M.to_int opt) vdd gnd
;;

(** Sign extend an immediate to ~to_ bits, filling the msbs with all ones or
    all zeros depending on the sign bit of the original value. *)
let sign_extend ~width t =
  let sign_bit = msb t in
  let extended_bits = width - Signal.width t in
  concat_msb [ mux2 sign_bit (ones extended_bits) (zero extended_bits); t ]
;;
