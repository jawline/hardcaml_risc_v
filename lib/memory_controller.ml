open Core
open Hardcaml
open Signal
open Always

module Make (M : sig
  val num_bytes : int
  val num_channels : int
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

  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; ch_to_controller : 'a Tx_bus.Tx.t [@length M.num_channels]
      ; (* We ignore the ready signal from the ch when responding, we do not permit pushback. Consider dropping this input. *)
        controller_to_ch : 'a Rx_bus.Rx.t [@length M.num_channels]
      }
  end

  module O = struct
    type 'a t =
      { ch_to_controller : 'a Tx_bus.Rx.t [@length M.num_channels]
      ; controller_to_ch : 'a Rx_bus.Tx.t [@length M.num_channels]
      }
  end

  let create scope (i : _ I.t) =
    let reg_spec = Reg_spec.create ~clock ~clear in
    let which_ch =
      reg_fb ~width:8 (Signal.mod_counter ~max:(M.num_channels - 1)) reg_spec
    in
    let last_ch = reg reg_spec which_ch in
    let which_ch_to_controller = Tx_bus.Tx.mux which_ch i.ch_to_controller in
    let memory =
      Ram.create
        ~collision_mode:Read_before_write
        ~size:(M.num_bytes * 8)
        ~write_ports:
          [| { write_enable =
                 which_ch_to_controller.valid &: which_ch_to_controller.data.write
             ; write_address = which_ch_co_controller.data.addr
             ; write_data = which_ch_co_controller.data.data
             ; write_clock = i.clock
             }
          |]
        ~read_ports:
          [| { read_enable =
                 which_ch_to_controller.valid &: ~:(which_ch_to_controller.data.write)
             ; read_address = which_ch_co_controller.data.addr
             ; read_clock = i.clock
             }
          |]
    in
    let read_data = memory.(0) in
    { O.ch_to_controller =
        List.init
          ~f:(fun channel ->
            (* Set ready for the channel we're considering
                        in round robin. *)
            Tx_bus.Rx.mux
              (which_ch ==:. channel)
              [ Tx_bus.Rx.Of_signal.of_int 1; Tx_bus.Rx.Of_signal.of_int 0 ])
          M.num_channels
    ; controller_to_ch =
        List.init
          ~f:(fun channel -> Rx_bus.Tx.mux (last_ch ==:. channel) (assert false))
          M.num_channels
    }
  ;;
end
