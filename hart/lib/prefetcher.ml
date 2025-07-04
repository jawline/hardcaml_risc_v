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

  let create
        scope
        ({ clock; clear; valid; aligned_address; read_bus; read_response } : _ I.t)
    =
    let reg_spec = Reg_spec.create ~clock ~clear () in
    let%hw fetching =
      valid |: reg_fb ~width:1 ~f:(fun t -> mux2 read_bus.ready gnd (t |: valid)) reg_spec
    in
    (* We could end up with multiple reads in flight so we buffer them up  *)
    let address_fifo =
      Fifo.create
        ~clock
        ~clear
        ~showahead:true
        ~read_latency:0
        ~capacity:8
        ~wr:read_bus.ready
        ~d:aligned_address
        ~rd:read_response.valid
        ()
    in
    { O.read_bus =
        { Memory.Read_bus.Source.valid = fetching &: ~:(address_fifo.full)
        ; data = { address = cut_through_latch ~enable:valid reg_spec aligned_address }
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
