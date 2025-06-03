open! Core
module Make (C : Onehot_intf.Config) : Onehot_intf.S with type base := C.t
