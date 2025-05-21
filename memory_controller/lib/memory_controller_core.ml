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
     end) =
struct
  open Memory_bus

  let () =
    if M.data_bus_width % 8 <> 0 then raise_s [%message "BUG: data bus must be in bytes"]
  ;;

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
  let unaligned_bits = Int.floor_log2 (M.data_bus_width / 8)

  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; which_read_ch : 'a [@bits num_bits_to_represent M.num_read_channels - 1]
      ; selected_read_ch : 'a Memory_bus.Read_bus.Source.t
      ; which_write_ch : 'a [@bits num_bits_to_represent M.num_write_channels - 1]
      ; selected_write_ch : 'a Memory_bus.Write_bus.Source.t
      }
    [@@deriving hardcaml ~rtlmangle:"$"]
  end

  module O = struct
    type 'a t =
      { read_response : 'a Read_response.With_valid.t list [@length M.num_read_channels]
      ; write_response : 'a Write_response.With_valid.t list
            [@length M.num_write_channels]
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

  let create
        ~read_latency
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
    let memory =
      Ram.create
        ~name:"main_memory_bram"
        ~collision_mode:Read_before_write
        ~size:capacity_in_words
        ~write_ports:
          [| { write_enable =
                 selected_write_ch.valid
                 &: ~:(illegal_operation ~scope selected_write_ch.data.address)
             ; write_address =
                 real_address ~scope selected_write_ch.data.address
                 |> sel_bottom ~width:address_width
             ; write_data = selected_write_ch.data.write_data
             ; write_clock = clock
             }
          |]
        ~read_ports:
          [| { read_enable =
                 selected_read_ch.valid
                 &: ~:(illegal_operation ~scope selected_read_ch.data.address)
             ; read_address =
                 real_address ~scope selected_read_ch.data.address
                 |> sel_bottom ~width:address_width
             ; read_clock = clock
             }
          |]
        ()
    in
    let%hw read_data = memory.(0) in
    { O.read_response =
        List.init
          ~f:(fun channel ->
            { With_valid.valid =
                (let read_was_valid =
                   reg reg_spec_with_clear selected_read_ch.valid
                   &: (reg reg_spec_with_clear which_read_ch ==:. channel)
                 in
                 pipeline ~n:(read_latency - 1) reg_spec_no_clear read_was_valid)
            ; value =
                { Read_response.error =
                    (let read_was_error =
                       reg
                         reg_spec_with_clear
                         (illegal_operation ~scope selected_read_ch.data.address)
                       &: (reg reg_spec_with_clear which_read_ch ==:. channel)
                     in
                     pipeline ~n:(read_latency - 1) reg_spec_no_clear read_was_error)
                ; read_data = pipeline ~n:(read_latency - 1) reg_spec_no_clear read_data
                }
            })
          M.num_read_channels
    ; write_response =
        List.init
          ~f:(fun channel ->
            { With_valid.valid =
                reg reg_spec_with_clear selected_write_ch.valid
                &: (reg reg_spec_with_clear which_write_ch ==:. channel)
            ; value =
                { Write_response.error =
                    reg
                      reg_spec_with_clear
                      (illegal_operation ~scope selected_write_ch.data.address)
                    &: (reg reg_spec_with_clear which_write_ch ==:. channel)
                }
            })
          M.num_write_channels
    }
  ;;

  let hierarchical ~instance ~read_latency (scope : Scope.t) (input : Signal.t I.t) =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical
      ~scope
      ~name:"memory_controller_core"
      ~instance
      (create ~read_latency)
      input
  ;;
end
