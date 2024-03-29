open! Core
open Hardcaml
open Signal

let mux_shift ~f ~filler signal by_signal =
  mux2 (by_signal >:. 31) filler (mux_init ~f:(fun by -> f signal by) by_signal 32)
;;

let srl = mux_shift ~f:srl ~filler:(zero 32)
let sll = mux_shift ~f:sll ~filler:(zero 32)
let sra = mux_shift ~f:sra ~filler:(ones 32)

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

let switch2 (type a) (module M : Switchable with type t = a) ~f ~if_not_found signal =
  let open M in
  ( mux_init
      ~f:(fun i -> of_int i |> Option.map ~f |> Option.value ~default:if_not_found |> fst)
      signal
      (Int.pow 2 (width signal))
  , mux_init
      ~f:(fun i -> of_int i |> Option.map ~f |> Option.value ~default:if_not_found |> snd)
      signal
      (Int.pow 2 (width signal)) )
;;

let is (type a) (module M : Switchable with type t = a) signal (opt : M.t) =
  mux2 (signal ==:. M.to_int opt) vdd gnd
;;
