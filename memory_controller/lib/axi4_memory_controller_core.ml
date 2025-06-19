open Core
open Hardcaml
open Signal

module Make
    (Memory_bus : Memory_bus_intf.S)
    (M : sig
       val capacity_in_bytes : int
       val num_read_channels : int
       val num_write_channels : int
       val data_bus_width : int
     end)
    (Config : Axi4_config_intf.Config) =
struct
  open Memory_bus

  let () =
    if M.data_bus_width % 8 <> 0 then raise_s [%message "BUG: data bus must be in bytes"]
  ;;

  let data_bus_in_bytes = M.data_bus_width / 8

  let () =
    if M.capacity_in_bytes % data_bus_in_bytes <> 0
    then
      raise_s
        [%message
          "BUG: cannot request a capacity that is not a multiple of data_bus_width"]
  ;;

  let capacity_in_words = M.capacity_in_bytes / data_bus_in_bytes
  let address_width = address_bits_for capacity_in_words
  let unaligned_bits = Int.floor_log2 (M.data_bus_width / 8)

  module Axi4 = Axi4.Make (Config)

  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; which_read_ch : 'a [@bits address_bits_for M.num_read_channels]
      ; selected_read_ch : 'a Memory_bus.Read_bus.Source.t
      ; which_write_ch : 'a [@bits address_bits_for M.num_write_channels]
      ; selected_write_ch : 'a Memory_bus.Write_bus.Source.t
      ; ddr : 'a Axi4.I.t
      }
    [@@deriving hardcaml ~rtlmangle:"$"]
  end

  module O = struct
    type 'a t =
      { read_response : 'a Read_response.With_valid.t list [@length M.num_read_channels]
      ; write_response : 'a Write_response.With_valid.t list
            [@length M.num_write_channels]
      ; ddr : 'a Axi4.O.t
      }
    [@@deriving hardcaml ~rtlmangle:"$"]
  end

  let real_address ~scope address =
    let%hw base_address = address in
    srl ~by:unaligned_bits base_address
  ;;

  let illegal_operation ~scope address =
    let%hw is_unaligned = sel_bottom ~width:unaligned_bits address <>:. 0 in
    let%hw is_out_of_range = real_address ~scope address >=:. capacity_in_words in
    is_unaligned |: is_out_of_range
  ;;

  let data_bus_elements_in_memory_bus = Config.data_width / data_bus_width

  let create
        scope
        ({ clock
         ; clear
         ; which_read_ch
         ; selected_read_ch
         ; which_write_ch
         ; selected_write_ch
         } :
          _ I.t)
    =
    let reg_spec_with_clear = Reg_spec.create ~clock ~clear () in
    let reg_spec_no_clear = Reg_spec.create ~clock () in
    let%hw read_data = assert false in
    { O.read_response = assert false; write_response = assert false; ddr = assert false }
  ;;

  let hierarchical (scope : Scope.t) (input : Signal.t I.t) =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ~scope ~name:"memory_controller_core" create input
  ;;
end
