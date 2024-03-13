open! Core
open Hardcaml
open Signal

let mux_shift ~f ~filler signal by_signal =
  mux2 (by_signal >:. 31) filler (mux_init ~f:(fun by -> f signal by) by_signal 32)
;;

let srl = mux_shift ~f:srl ~filler:(zero 32)
let sll = mux_shift ~f:sll ~filler:(zero 32)
let sra = mux_shift ~f:sra ~filler:(ones 32)
