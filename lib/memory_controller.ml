open Core
open Hardcaml
open Signal

module Make (M : sig
    val num_bytes : int
    val num_channels : int
    val address_width : int
    val data_bus_width : int
  end) =
struct
  let () =
    if M.data_bus_width % 8 <> 0 then raise_s [%message "BUG: data bus must be in bytes"]
  ;;

  include Memory_bus.Make (M)

  let data_bus_in_bytes = M.data_bus_width / 8

  let () =
    if M.num_bytes % data_bus_in_bytes <> 0
    then
      raise_s
        [%message
          "BUG: cannot request num_bytes that is not a multiple of data_bus_width"]
  ;;

  let desired_bytes_in_words = M.num_bytes / data_bus_in_bytes

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
      ; ch_to_controller : 'a Tx_bus.Tx.t list
           [@length M.num_channels] [@rtlprefix "ch_co_controller"]
      ; (* We ignore the ready signal from the ch when responding, we do not permit pushback. Consider dropping this input. *)
        controller_to_ch : 'a Rx_bus.Rx.t list
           [@length M.num_channels] [@rtlprefix "controller_to_ch"]
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t =
      { ch_to_controller : 'a Tx_bus.Rx.t list
           [@length M.num_channels] [@rtlprefix "ch_co_controller"]
      ; controller_to_ch : 'a Rx_bus.Tx.t list
           [@length M.num_channels] [@rtlprefix "controller_to_ch"]
      }
    [@@deriving sexp_of, hardcaml]
  end

  let create scope (i : _ I.t) =
    let ( -- ) = Scope.naming scope in
    let reg_spec = Reg_spec.create ~clock:i.clock ~clear:i.clear () in
    let which_ch =
      reg_fb ~width:8 ~f:(mod_counter ~max:(M.num_channels - 1)) reg_spec -- "which_ch"
    in
    let last_ch = reg reg_spec which_ch -- "last_ch " in
    let which_ch_to_controller =
      if M.num_channels = 1
      then List.hd_exn i.ch_to_controller
      else Tx_bus.Tx.Of_signal.mux which_ch i.ch_to_controller
    in
    let unaligned_bits = Int.floor_log2 (M.data_bus_width / 8) in
    printf "%i\n" unaligned_bits;
    (* We truncate the address by unaligned bits to get the address in words. *)
    let real_address =
      srl which_ch_to_controller.data.address unaligned_bits -- "real_address"
    in
    let is_operation = which_ch_to_controller.valid -- "is_operation" in
    let illegal_operation =
      let is_unaligned = which_ch_to_controller.data.address &:. unaligned_bits <>:. 0 in
      let is_out_of_range = real_address >:. desired_bytes_in_words in
      (is_operation &: (is_unaligned |: is_out_of_range)) -- "illegal_operation"
    in
    let was_error = reg reg_spec illegal_operation -- "was_error" in
    let is_operation_and_is_legal =
      (is_operation &: ~:illegal_operation) -- "is_operation_and_is_legal"
    in
    let is_write = which_ch_to_controller.data.write -- "is_write_operation" in
    let was_operation =
      (* We set valid to high when a read / write is completed. This will be on
         the next cycle from acknowledgement. *)
      is_operation_and_is_legal |> reg reg_spec
    in
    let memory =
      Ram.create
        ~name:"main_memory_bram"
        ~collision_mode:Read_before_write
        ~size:desired_bytes_in_words
        ~write_ports:
          [| { write_enable =
                 (is_operation_and_is_legal &: is_write) -- "ram$write_enable"
             ; write_address = real_address -- "ram$write_address"
             ; write_data = which_ch_to_controller.data.write_data -- "ram$write_data"
             ; write_clock = i.clock
             }
          |]
        ~read_ports:
          [| { read_enable =
                 (is_operation_and_is_legal &: ~:is_write) -- "ram$read_enable"
             ; read_address = real_address -- "ram$read_address"
             ; read_clock = i.clock
             }
          |]
        ()
    in
    let read_data = memory.(0) -- "ram$read_data" in
    { O.ch_to_controller =
        List.init
          ~f:(fun channel ->
            (* Set ready for the channel we're considering in round robin. *)
            Tx_bus.Rx.Of_signal.mux
              (which_ch ==:. channel)
              [ Tx_bus.Rx.Of_signal.of_int 0; Tx_bus.Rx.Of_signal.of_int 1 ])
          M.num_channels
    ; controller_to_ch =
        List.init
          ~f:(fun channel ->
            Rx_bus.Tx.Of_signal.mux
              (last_ch ==:. channel)
              [ Rx_bus.Tx.Of_signal.of_int 0
              ; { Rx_bus.Tx.valid = was_operation
                ; data = { error = was_error; read_data }
                }
              ])
          M.num_channels
    }
  ;;

  let hierarchical ~instance (scope : Scope.t) (input : Signal.t I.t) =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ~scope ~name:"Memory_controller" ~instance create input
  ;;
end
