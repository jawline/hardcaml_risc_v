open Core
open Hardcaml
open Hardcaml_stream
open Signal

module Make (M : sig
    val capacity_in_bytes : int
    val num_channels : int
    val address_width : int
    val data_bus_width : int
  end) =
struct
  let () =
    if M.data_bus_width % 8 <> 0 then raise_s [%message "BUG: data bus must be in bytes"]
  ;;

  include Memory_bus.Make (M)

  let data_bus_width = M.data_bus_width
  let data_bus_in_bytes = M.data_bus_width / 8

  let () =
    if M.capacity_in_bytes % data_bus_in_bytes <> 0
    then
      raise_s
        [%message
          "BUG: cannot request a capacity that is not a multiple of data_bus_width"]
  ;;

  let capacity_in_words = M.capacity_in_bytes / data_bus_in_bytes
  let address_width = num_bits_to_represent (capacity_in_words - 1)

  module Tx_data = struct
    type 'a t =
      { write : 'a
      ; address : 'a [@bits M.address_width]
      ; write_data : 'a [@bits M.data_bus_width]
      }
    [@@deriving sexp_of, hardcaml ~rtlmangle:"$"]
  end

  module Rx_data = struct
    type 'a t =
      { error : 'a
      ; read_data : 'a [@bits M.data_bus_width]
      }
    [@@deriving sexp_of, hardcaml ~rtlmangle:"$"]
  end

  module Tx_bus = Stream.Make (Tx_data)
  module Rx_bus = Stream.Make (Rx_data)

  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; ch_to_controller : 'a Tx_bus.Tx.t list [@length M.num_channels]
      }
    [@@deriving sexp_of, hardcaml ~rtlmangle:"$"]
  end

  module O = struct
    type 'a t =
      { ch_to_controller : 'a Tx_bus.Rx.t list [@length M.num_channels]
      ; controller_to_ch : 'a Rx_bus.Tx.t list [@length M.num_channels]
      }
    [@@deriving sexp_of, hardcaml ~rtlmangle:"$"]
  end

  let rotate n xs = List.(concat [ drop xs n; take xs n ])

  let round_robin_priority_select ~clock ~ch_to_controller scope =
    let reg_spec_no_clear = Reg_spec.create ~clock () in
    let%hw round_robin =
      reg_fb
        ~width:(Signal.num_bits_to_represent (M.num_channels - 1))
        ~f:(mod_counter ~max:(M.num_channels - 1))
        reg_spec_no_clear
    in
    let channels =
      List.mapi
        ~f:(fun (ch : int) (t : Signal.t Tx_bus.Tx.t) : Signal.t With_valid.t ->
          { With_valid.valid = t.valid
          ; value = of_int ~width:(num_bits_to_represent (M.num_channels - 1)) ch
          })
        ch_to_controller
    in
    mux_init
      ~f:(fun (ch : int) -> (rotate ch channels |> priority_select).value)
      round_robin
      M.num_channels
  ;;

  let create scope ({ clock; clear = _; ch_to_controller } : _ I.t) =
    let reg_spec_no_clear = Reg_spec.create ~clock () in
    let%hw which_ch =
      if M.num_channels = 1
      then gnd
      else round_robin_priority_select ~clock ~ch_to_controller scope
    in
    let which_ch_to_controller =
      if M.num_channels = 1
      then List.hd_exn ch_to_controller
      else Tx_bus.Tx.Of_signal.mux which_ch ch_to_controller
    in
    let unaligned_bits = Int.floor_log2 (M.data_bus_width / 8) in
    (* We truncate the address by unaligned bits to get the address in words. *)
    let%hw real_address =
      let%hw base_address = which_ch_to_controller.data.address in
      srl ~by:unaligned_bits base_address
    in
    let%hw is_operation = which_ch_to_controller.valid in
    let%hw illegal_operation =
      let%hw is_unaligned =
        sel_bottom ~width:unaligned_bits which_ch_to_controller.data.address <>:. 0
      in
      let%hw is_out_of_range = real_address >=:. capacity_in_words in
      is_operation &: (is_unaligned |: is_out_of_range)
    in
    let was_error = reg reg_spec_no_clear illegal_operation in
    let%hw is_operation_and_is_legal = is_operation &: ~:illegal_operation in
    let%hw is_write = which_ch_to_controller.data.write in
    let memory =
      Ram.create
        ~name:"main_memory_bram"
        ~collision_mode:Read_before_write
        ~size:capacity_in_words
        ~write_ports:
          [| { write_enable = is_operation_and_is_legal &: is_write
             ; write_address = sel_bottom ~width:address_width real_address
             ; write_data = which_ch_to_controller.data.write_data
             ; write_clock = clock
             }
          |]
        ~read_ports:
          [| { read_enable = is_operation_and_is_legal &: ~:is_write
             ; read_address = sel_bottom ~width:address_width real_address
             ; read_clock = clock
             }
          |]
        ()
    in
    let%hw read_data = memory.(0) in
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
            { Rx_bus.Tx.valid =
                reg reg_spec_no_clear (which_ch ==:. channel &: is_operation)
            ; data = { error = was_error; read_data }
            })
          M.num_channels
    }
  ;;

  let hierarchical ~instance (scope : Scope.t) (input : Signal.t I.t) =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ~scope ~name:"memory_controller" ~instance create input
  ;;
end
