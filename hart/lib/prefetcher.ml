open! Core
open Hardcaml
open Hardcaml_memory_controller
open Signal

module Make (Memory : Memory_bus_intf.S) = struct
  let address_width = Memory.Read_bus.Source.port_widths.data.address
  let data_width = Memory.Read_response.port_widths.read_data

  module I = struct
    type 'a t =
      { clock : 'a Clocking.t
      ; valid : 'a [@rtlprefix "input_"]
      ; aligned_address : 'a [@bits address_width] [@rtlprefix "input_"]
      ; read_bus : 'a Memory.Read_bus.Dest.t
      ; read_response : 'a Memory.Read_response.With_valid.t
      }
    [@@deriving hardcaml ~rtlmangle:"$"]
  end

  module O = struct
    type 'a t =
      { valid : 'a
      ; aligned_address : 'a [@bits address_width]
      ; value : 'a [@bits data_width]
      ; read_bus : 'a Memory.Read_bus.Source.t
      ; ready : 'a
      }
    [@@deriving hardcaml ~rtlmangle:"$"]
  end

  let cut_through_latch ~enable reg_spec signal =
    mux2 enable signal (reg ~enable reg_spec signal)
  ;;

  module State = struct
    type t =
      | Idle
      | Fetch
      | Fetch_next_guess
    [@@deriving enumerate, sexp, compare ~localize]
  end

  let create scope ({ clock; valid; aligned_address; read_bus; read_response } : _ I.t) =
    let reg_spec = Clocking.to_spec clock in
    let%hw.Always.State_machine sm =
      Always.State_machine.create (module State) reg_spec
    in
    let%hw aligned_address = cut_through_latch ~enable:valid reg_spec aligned_address in
    let%hw next_guess = reg ~enable:valid reg_spec (aligned_address +:. 1) in
    let%hw fetching =
      (* TODO: We could cut through on valid to increase throughput here. *)
      sm.is Fetch |: sm.is Fetch_next_guess
    in
    let address_fifo_full = wire 1 in
    let will_fetch = fetching &: ~:address_fifo_full in
    (* We could end up with multiple reads in flight so we buffer them up  *)
    let address_fifo =
      Fifo.create
        ~showahead:true
        ~clock:clock.clock
        ~clear:clock.clear
        ~capacity:8
        ~wr:(will_fetch &: read_bus.ready)
        ~d:(mux2 (sm.is Fetch_next_guess) next_guess aligned_address)
        ~rd:read_response.valid
        ()
    in
    address_fifo_full <-- address_fifo.full;
    let%hw prefetched_valid =
      cut_through_latch ~enable:read_response.valid reg_spec read_response.valid
    in
    let%hw prefetched_address =
      cut_through_latch ~enable:read_response.valid reg_spec address_fifo.q
    in
    let%hw prefetched_result =
      cut_through_latch ~enable:read_response.valid reg_spec read_response.value.read_data
    in
    Always.(
      compile
        [ sm.switch
            [ ( Idle
              , [ when_
                    valid
                    [ if_
                        (prefetched_valid &: (prefetched_address ==: aligned_address))
                        [ sm.set_next Fetch_next_guess ]
                      @@ else_ [ sm.set_next Fetch ]
                    ]
                ] )
            ; Fetch, [ when_ read_bus.ready [ sm.set_next Fetch_next_guess ] ]
            ; Fetch_next_guess, [ when_ read_bus.ready [ sm.set_next Idle ] ]
            ]
        ]);
    { O.read_bus =
        { Memory.Read_bus.Source.valid = will_fetch
        ; data = { address = mux2 (sm.is Fetch_next_guess) next_guess aligned_address }
        }
    ; valid = (prefetched_valid &: (aligned_address ==: prefetched_address))
    ; aligned_address = prefetched_address
    ; value = prefetched_result
    ; ready = sm.is Idle
    }
  ;;

  let hierarchical (scope : Scope.t) (input : Signal.t I.t) =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ~scope ~name:"prefetcher" create input
  ;;
end
