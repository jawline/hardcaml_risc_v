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
  let unaligned_bits = address_bits_for (M.data_bus_width / 8)

  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; which_read_ch : 'a [@bits address_bits_for M.num_read_channels]
      ; selected_read_ch : 'a Memory_bus.Read_bus.Source.t
      ; which_write_ch : 'a [@bits address_bits_for M.num_write_channels]
      ; selected_write_ch : 'a Memory_bus.Write_bus.Source.t
      }
    [@@deriving hardcaml ~rtlmangle:"$"]
  end

  module O = struct
    type 'a t =
      { read_response : 'a Read_response.With_valid.t list [@length M.num_read_channels]
      ; read_error : 'a
      ; write_response : 'a Write_response.With_valid.t list
            [@length M.num_write_channels]
      ; write_error : 'a
      }
    [@@deriving hardcaml ~rtlmangle:"$"]
  end

  let real_address ~scope address =
    let%hw base_address = address in
    drop_bottom ~width:unaligned_bits base_address
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
    let%hw bram_write_address =
      real_address ~scope selected_write_ch.data.address
      |> sel_bottom ~width:address_width
    in
    let%hw bram_read_address =
      real_address ~scope selected_read_ch.data.address |> sel_bottom ~width:address_width
    in
    let%hw read_illegal =
      illegal_operation
        ~scope:(Scope.sub_scope scope "read")
        selected_read_ch.data.address
    in
    let%hw write_illegal =
      illegal_operation
        ~scope:(Scope.sub_scope scope "write")
        selected_write_ch.data.address
    in
    let memory =
      Ram.create
        ~name:"main_memory_bram"
        ~collision_mode:Read_before_write
        ~size:capacity_in_words
        ~write_ports:
          [| { write_enable = selected_write_ch.valid &: ~:write_illegal
             ; write_address = bram_write_address
             ; write_data = selected_write_ch.data.write_data
             ; write_clock = clock
             }
          |]
        ~read_ports:
          [| { read_enable = selected_read_ch.valid &: ~:read_illegal
             ; read_address = bram_read_address
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
                pipeline
                  ~n:read_latency
                  reg_spec_with_clear
                  (selected_read_ch.valid
                   &: (which_read_ch ==:. channel)
                   &: ~:read_illegal)
            ; value =
                { Read_response.read_data =
                    pipeline ~n:(read_latency - 1) reg_spec_no_clear read_data
                }
            })
          M.num_read_channels
    ; read_error = read_illegal
    ; write_response =
        List.init
          ~f:(fun channel ->
            { With_valid.valid =
                reg
                  reg_spec_with_clear
                  (selected_write_ch.valid
                   &: (which_write_ch ==:. channel)
                   &: ~:write_illegal)
            ; value = { Write_response.dummy = gnd }
            })
          M.num_write_channels
    ; write_error = write_illegal
    }
  ;;

  let hierarchical ~read_latency (scope : Scope.t) (input : Signal.t I.t) =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ~scope ~name:"memory_controller_core" (create ~read_latency) input
  ;;
end
