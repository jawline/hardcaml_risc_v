open! Core
open! Hardcaml
open! Signal

module Make (Hart_config : Hart_config_intf.S) = struct
  let register_width = Register_width.bits Hart_config.register_width

  type 'a t =
    { pc : 'a [@bits register_width]
    ; general : 'a list [@bits register_width] [@length Hart_config.num_registers]
    }
  [@@deriving sexp_of, hardcaml]

  let set_pc t new_pc = { t with pc = new_pc }

  let assign_when ~when_ ~index_signal ~value_signal t =
    { t with
      general =
        List.mapi
          ~f:(fun index current_value ->
            if index = 0
            then current_value
            else mux2 (when_ &: (index_signal ==:. index)) value_signal current_value)
          t.general
    }
  ;;

  (** x0 is hardwired to zero so it is not in the writeback. *)
  module For_writeback = struct
    type nonrec 'a registers = 'a t

    type 'a t =
      { pc : 'a [@bits register_width]
      ; general : 'a list [@bits register_width] [@length Hart_config.num_registers - 1]
      }
    [@@deriving sexp_of, hardcaml]

    let to_registers (t : _ t) : _ registers =
      { pc = t.pc; general = zero register_width :: t.general }
    ;;

    let of_registers (t : _ registers) : _ t =
      { pc = t.pc; general = List.drop t.general 1 }
    ;;
  end
end
