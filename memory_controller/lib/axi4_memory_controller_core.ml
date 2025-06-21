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

  (** There are two alignments of the base address. The base address is host addressed (bytes),
      our core requires that memory be aligned to a word address (4 bytes / 8 bytes) and the AXI4
      interface has a wider addressing scheme (usually 32 / 64 bytes). *)
  let unaligned_bits_data_bus = address_bits_for (M.data_bus_width / 8)

  let () =
    if Config.data_width <> M.data_bus_width
    then
      raise_s
        [%message "BUG: currently bus widths different to axi width are not supported"]
  ;;

  module Axi4 = Axi4.Make (Config)

  let () =
    if Config.id_width < M.num_read_channels
    then
      raise_s
        [%message
          "BUG: The AXI4 MIG config should have an ID width large enough to address each \
           read channel"];
    if Config.id_width < M.num_write_channels
    then
      raise_s
        [%message
          "BUG: The AXI4 MIG config should have an ID with large enough to address each \
           write channel"];
    ()
  ;;

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
      ; read_ready : 'a
      ; read_error : 'a
      ; write_response : 'a Write_response.With_valid.t list
            [@length M.num_write_channels]
      ; write_ready : 'a
      ; write_error : 'a
      ; ddr : 'a Axi4.O.t
      }
    [@@deriving hardcaml ~rtlmangle:"$"]
  end

  let real_address ~scope address =
    let%hw base_address = address in
    drop_bottom ~width:unaligned_bits_data_bus base_address
  ;;

  let illegal_operation ~scope address =
    let%hw is_unaligned = sel_bottom ~width:unaligned_bits_data_bus address <>:. 0 in
    let%hw is_out_of_range = real_address ~scope address >=:. capacity_in_words in
    is_unaligned |: is_out_of_range
  ;;

  let create
        scope
        ({ clock = _
         ; clear = _
         ; which_read_ch
         ; selected_read_ch
         ; which_write_ch
         ; selected_write_ch
         ; ddr
         } :
          _ I.t)
    =
    let read_invalid = illegal_operation ~scope selected_read_ch.data.address in
    let write_invalid = illegal_operation ~scope selected_write_ch.data.address in
    { O.read_response =
        List.init
          ~f:(fun channel ->
            let is_channel = ddr.s_axi_rid ==:. channel in
            { With_valid.valid = ddr.s_axi_rvalid &: is_channel
            ; value = { Read_response.read_data = ddr.s_axi_rdata }
            })
          M.num_read_channels
    ; read_ready = ddr.s_axi_rready
    ; read_error = selected_read_ch.valid &: read_invalid
    ; write_response =
        List.init
          ~f:(fun channel ->
            let is_channel = ddr.s_axi_bid ==:. channel in
            { With_valid.valid = ddr.s_axi_bvalid &: is_channel
            ; value = { Write_response.dummy = gnd }
            })
          M.num_read_channels
    ; write_ready = ddr.s_axi_wready
    ; write_error = selected_write_ch.valid
    ; ddr =
        { s_axi_awvalid = selected_write_ch.valid &: ~:write_invalid
        ; s_axi_awid = which_write_ch
        ; s_axi_awaddr =
            drop_bottom ~width:unaligned_bits_data_bus selected_write_ch.data.address
        ; s_axi_awlena = one 8
        ; s_axi_awsize = one 3
        ; s_axi_awburst = zero 2
        ; s_axi_wdata = selected_write_ch.data.write_data
        ; s_axi_wstrb = selected_write_ch.data.wstrb
        ; s_axi_wlast = vdd
        ; s_axi_arvalid = selected_read_ch.valid &: ~:read_invalid
        ; s_axi_arid = which_read_ch
        ; s_axi_araddr =
            drop_bottom ~width:unaligned_bits_data_bus selected_read_ch.data.address
        ; s_axi_arlen = one 8
        ; s_axi_arsize = one 3
        ; s_axi_arburst = zero 2
        ; s_axi_rready = vdd
        }
    }
  ;;

  let hierarchical (scope : Scope.t) (input : Signal.t I.t) =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ~scope ~name:"memory_controller_core" create input
  ;;
end
