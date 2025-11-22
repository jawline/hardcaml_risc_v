open! Core
open Hardcaml
open Hardcaml_memory_controller
open Signal

module Make (Memory : Memory_bus_intf.S) = struct
  let address_width = Memory.Read_bus.Source.port_widths.data.address
  let data_width = Memory.Read_response.port_widths.read_data

  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
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

  let create
        scope
        ({ clock; clear; valid; aligned_address; read_bus; read_response } : _ I.t)
    =
    let reg_spec = Reg_spec.create ~clock ~clear () in
    let aligned_address = cut_through_latch ~enable:valid reg_spec aligned_address in
    let%hw.Always.State_machine sm =
      Always.State_machine.create (module State) reg_spec
    in
    let next_guess = reg ~enable:valid reg_spec (aligned_address +:. 1) in
    let%hw fetching = valid |: sm.is Fetch |: sm.is Fetch_next_guess in
    let address_fifo_full = wire 1 in
    let will_fetch = fetching &: ~:address_fifo_full in
    (* We could end up with multiple reads in flight so we buffer them up  *)
    let address_fifo =
      Fifo.create
        ~clock
        ~clear
        ~showahead:true
        ~read_latency:0
        ~capacity:8
        ~wr:(will_fetch &: read_bus.ready)
        ~d:(mux2 (sm.is Fetch_next_guess) next_guess aligned_address)
        ~rd:read_response.valid
        ()
    in
    address_fifo_full <-- address_fifo.full;
    Always.(
      compile
        [ if_
            valid
            [ if_ read_bus.ready [ sm.set_next Fetch_next_guess ]
              @@ else_ [ sm.set_next Fetch ]
            ]
          @@ else_
               [ sm.switch
                   [ Idle, []
                   ; Fetch, [ when_ read_bus.ready [ sm.set_next Fetch_next_guess ] ]
                   ; Fetch_next_guess, [ when_ read_bus.ready [ sm.set_next Idle ] ]
                   ]
               ]
        ]);
    { O.read_bus =
        { Memory.Read_bus.Source.valid = will_fetch
        ; data = { address = mux2 (sm.is Fetch_next_guess) next_guess aligned_address }
        }
    ; valid = cut_through_latch ~enable:read_response.valid reg_spec read_response.valid
    ; aligned_address =
        cut_through_latch ~enable:read_response.valid reg_spec address_fifo.q
    ; value =
        cut_through_latch
          ~enable:read_response.valid
          reg_spec
          read_response.value.read_data
    ; ready = read_bus.ready
    }
  ;;

  let hierarchical (scope : Scope.t) (input : Signal.t I.t) =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ~scope ~name:"prefetcher" create input
  ;;
end
