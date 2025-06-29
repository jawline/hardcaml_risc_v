open! Core
open Hardcaml
open Hardcaml_axi
open Hardcaml_memory_controller
open Signal
open Always

module Make
    (Config : Memory_to_packet8_intf.Config)
    (Memory : Memory_bus_intf.S)
    (Axi : Stream.S) =
struct
  let address_width = Memory.address_width
  let data_width = Memory.data_bus_width
  let bytes_per_beat = data_width / 8

  module Input = struct
    module T = struct
      type 'a t =
        { length : 'a [@bits 16]
        ; address : 'a [@bits address_width]
        }
      [@@deriving hardcaml ~rtlmangle:"$"]
    end

    include T
    module With_valid = With_valid.Wrap.Make (T)
  end

  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; enable : 'a Input.With_valid.t
      ; output_packet : 'a Axi.Dest.t
      ; memory : 'a Memory.Read_bus.Dest.t
      ; memory_response : 'a Memory.Read_response.With_valid.t
      }
    [@@deriving hardcaml ~rtlmangle:"$"]
  end

  module O = struct
    type 'a t =
      { ready : 'a
      ; output_packet : 'a Axi.Source.t [@rtlprefix "output$"]
      ; memory : 'a Memory.Read_bus.Source.t [@rtlprefix "memory$"]
      }
    [@@deriving hardcaml ~rtlmangle:"$"]
  end

  let () = assert (Axi.Source.port_widths.tdata = 8)

  module State = struct
    type t =
      | Idle
      | Writing_header
      | Writing_length
      | Reading_data
      | Writing_data
    [@@deriving sexp, enumerate, compare]
  end

  let create
        (scope : Scope.t)
        ({ I.clock
         ; clear
         ; enable =
             { valid = input_enable
             ; value = { length = input_length; address = input_address }
             }
         ; memory_response
         ; output_packet = { tready = output_packet_ready }
         ; memory = memory_ack
         } :
          _ I.t)
    =
    let reg_spec = Reg_spec.create ~clock ~clear () in
    let reg_spec_no_clear = Reg_spec.create ~clock () in
    let%hw.State_machine state = State_machine.create (module State) reg_spec in
    let%hw_var do_read = Variable.reg ~width:1 reg_spec in
    let%hw_var length = Variable.reg ~width:(width input_length) reg_spec_no_clear in
    let%hw_var address = Variable.reg ~width:(width input_address) reg_spec_no_clear in
    let%hw_var which_step =
      Variable.reg ~width:(address_bits_for bytes_per_beat) reg_spec_no_clear
    in
    let%hw_var read_data =
      Variable.reg ~width:(width memory_response.value.read_data) reg_spec_no_clear
    in
    let enter_reading_data = proc [ state.set_next Reading_data; do_read <-- vdd ] in
    let reset = proc [ do_read <-- gnd; state.set_next Idle ] in
    let%hw aligned_address =
      (Memory.byte_address_to_memory_address input_address).value
    in
    let%hw unaligned_bits =
      reg
        ~enable:(state.is Idle)
        reg_spec_no_clear
        (sel_bottom ~width:(address_bits_for (data_width / 8)) input_address)
    in
    compile
      [ state.switch
          [ ( State.Idle
            , [ (* Guard against entering the state machine
                   when zero length is requested to avoid sending null
                   packets out. This isn't strictly necessary but makes
                   the state machine much easier to think about. *)
                reset
              ; when_
                  (input_enable &: (input_length <>:. 0))
                  [ length <-- input_length
                  ; address <-- aligned_address
                  ; which_step <--. 0
                  ; (match Config.header with
                     | Some _ -> Writing_header
                     | None -> Writing_length)
                    |> state.set_next
                  ]
              ] )
          ; ( Writing_header
            , match Config.header with
              | Some _header ->
                [ when_ output_packet_ready [ state.set_next Writing_length ] ]
              | None -> [] )
          ; ( Writing_length
            , [ when_
                  output_packet_ready
                  [ incr which_step
                  ; when_
                      (which_step.value ==:. 1)
                      [ (* If the address was unaligned, set which_step to the
                           offset here to align it. *)
                        which_step <-- unaligned_bits
                      ; enter_reading_data
                      ]
                  ]
              ] )
          ; ( Reading_data
            , [ (* We will lower the memory request when the memory controller
                   acks then wait for the response. *)
                when_ memory_ack.ready [ do_read <-- gnd ]
              ; (* There will only be one request in flight on our line so we
                   don't need to worry about other data. *)
                when_
                  memory_response.valid
                  [ (* Memory read can fail, if they do return zero. *)
                    read_data <-- memory_response.value.read_data
                  ; state.set_next Writing_data
                  ]
              ] )
          ; ( Writing_data
            , [ when_
                  output_packet_ready
                  [ decr length
                  ; incr which_step
                  ; (* TODO: Once we have exhausted our read, we return
                       to reading data. We could prefetch here to speed this
                       up and avoid the stall. *)
                    when_
                      (which_step.value ==:. bytes_per_beat - 1)
                      [ which_step <--. 0; incr address; enter_reading_data ]
                  ; (* If this was the last write, reset the entire state machine to idle. *)
                    when_ (length.value ==:. 1) [ reset ]
                  ]
              ] )
          ]
      ];
    { O.ready = state.is State.Idle
    ; output_packet =
        { tvalid =
            state.is Writing_header |: state.is Writing_length |: state.is Writing_data
        ; tdata =
            (let%hw which_data_byte =
               mux which_step.value (split_lsb ~part_width:8 read_data.value)
             in
             let%hw which_length_byte =
               mux which_step.value (split_msb ~part_width:8 length.value)
             in
             let base = mux2 (state.is Writing_data) which_data_byte which_length_byte in
             match Config.header with
             | Some header -> mux2 (state.is Writing_header) (Signal.of_char header) base
             | None -> base)
        ; tlast = state.is Writing_data &: (length.value ==:. 1)
        ; tkeep = ones 1
        ; tstrb = ones 1
        ; tuser = zero Axi.Source.port_widths.tuser
        }
    ; memory = { valid = do_read.value; data = { address = address.value } }
    }
  ;;

  let hierarchical (scope : Scope.t) (input : Signal.t I.t) =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ~scope ~name:"dma_memory_to_packet" create input
  ;;
end
