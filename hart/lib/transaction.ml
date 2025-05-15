(** A transaction encodes the desired state change from an opcode, including a
    finished flag to indicate the opcode is done if it spans multiple cycles. *)
open! Core

open! Hardcaml
open Hardcaml_memory_controller

module Make (Hart_config : Hart_config_intf.S) (Memory : Memory_bus_intf.S) = struct
  let register_width = Register_width.bits Hart_config.register_width

  module T = struct
    type 'a t =
      { (* TODO: We could pull old_rd into decoded instruction to avoid the gate on committing *)
        set_rd : 'a
      ; new_rd : 'a [@bits register_width]
      ; new_pc : 'a [@bits register_width]
      ; error : 'a
      }
    [@@deriving hardcaml ~rtlmangle:"$"]

    let generic_error =
      let open Signal in
      { set_rd = gnd
      ; new_rd = zero register_width
      ; new_pc = zero register_width
      ; error = vdd
      }
    ;;
  end

  include T
  module With_valid = With_valid.Wrap.Make (T)
end
