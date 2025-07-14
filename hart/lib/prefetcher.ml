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
      ; board_clear : 'a
      ; hart_clear : 'a
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
    [@@deriving enumerate, sexp, compare]
  end

  let address_fifo_depth = 16
  let lookahead_fifo_depth = 8

  let construct_address_fifo ~clock ~clear ~wr ~rd ~address =
    Fifo.create
      ~clock
      ~clear
      ~showahead:true
      ~read_latency:0
      ~capacity:address_fifo_depth
      ~wr
      ~d:address
      ~rd
      ()
  ;;

  let construct_result_fifo ~clock ~clear ~wr ~rd ~result ~address =
    (* TODO: Make this cut through? *)
    let fifo =
      Fifo.create
        ~clock
        ~clear
        ~showahead:true
        ~read_latency:0
        ~capacity:lookahead_fifo_depth
        ~wr
        ~rd
        ~d:(concat_lsb [ address; result ])
        ()
    in
    let q =
      sel_bottom ~width:(width address) fifo.q, drop_bottom ~width:(width address) fifo.q
    in
    q, fifo
  ;;

  let start_scheduling
        ~clock
        ~clear
        ~board_clear
        ~valid
        ~aligned_address
        ~(current_cache_address : t With_valid.t)
        ~read_ready
        ~result_fifo_ready
    =
    let spec = Reg_spec.create ~clock ~clear () in
    let we_are_ready = read_ready in
    let valid_matches_cache =
      current_cache_address.valid &: (current_cache_address.value ==: aligned_address)
    in
    let remaining_to_cache =
      reg_fb
        ~width:(address_bits_for lookahead_fifo_depth)
        ~f:(fun t ->
          let predecessor = mux2 (t ==:. 0) t (t -:. 1) in
          let reset_on_valid =
            mux2
              valid_matches_cache
              Uop.(t +:. 1)
              (of_unsigned_int ~width:(width t) (lookahead_fifo_depth - 1))
          in
          mux2 (valid &: read_ready) reset_on_valid predecessor)
        spec
    in
    let schedule_next_beat = valid |: (remaining_to_cache <>:. 0) in
    let next_address_to_issue =
      let reset = valid &: we_are_ready &: ~:valid_matches_cache in
      let address_register =
        (* Stores the next address when prefetching rather than driven by valid signal. *)
        reg_fb
          ~width:(width aligned_address)
          ~f:(fun t ->
            let reset_to = mux2 read_ready (aligned_address +:. 1) aligned_address in
            let prefetch_next_value = mux2 read_ready (t +:. 1) t in
            mux2 reset reset_to prefetch_next_value)
          spec
      in
      mux2 reset aligned_address address_register
    in
    let read_request =
      { Memory.Read_bus.Source.valid = schedule_next_beat
      ; data = { address = next_address_to_issue }
      }
    in
    let address_fifo =
      construct_address_fifo
        ~clock
        ~clear:board_clear
        ~wr:(schedule_next_beat &: read_ready)
        ~rd:vdd
        ~address:next_address_to_issue
    in
    assert false
  ;;

  let handle_streaming_results
        ~clock
        ~clear
        ~valid
        ~aligned_address
        ~read_valid
        ~read_address
        ~read_result
    =
    let spec = Reg_spec.create ~clock ~clear () in
    let cache_hit = assert false in
    let next_expected_address =
      reg_fb
        ~width:(width aligned_address)
        ~f:(fun t ->
          mux2 (valid ~:cache_hit) aligned_address (mux2 read_valid (t +:. 1) t))
        spec
    in
    let (result_address, result_data), fifo = assert false in
    assert false
  ;;

  let create
        scope
        ({ clock
         ; board_clear
         ; hart_clear
         ; valid
         ; aligned_address
         ; read_bus
         ; read_response
         } :
          _ I.t)
    =
    (* In general we clear on either board or hart clear, however the address
       fifos care about reads in flight so should only clear on board clear. *)
    let clear = board_clear |: hart_clear in
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
      construct_address_fifo
        ~clock
        ~clear
        ~wr:(will_fetch &: read_bus.ready)
        ~rd:read_response.valid
        ~address:(mux2 (sm.is Fetch_next_guess) next_guess aligned_address)
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
