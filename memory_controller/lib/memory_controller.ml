open! Core
open Hardcaml
open Signal

module Make
    (M : sig
       val data_bus_width : int
       val capacity_in_bytes : int
       val num_read_channels : int
       val num_write_channels : int
       val address_width : int
     end)
    (Axi_config : Axi4_config_intf.Config)
    (Axi : Axi4_intf.M(Axi_config).S) =
struct
  module Memory_bus = Memory_bus.Make (struct
      include M
    end)

  open Memory_bus

  module Read_arbitrator =
    Memory_channel_arbitrator.Make
      (Read_bus)
      (struct
        let num_channels = M.num_read_channels
      end)

  module Write_arbitrator =
    Memory_channel_arbitrator.Make
      (Write_bus)
      (struct
        let num_channels = M.num_write_channels
      end)

  module Core = Axi4_memory_controller_core.Make (Memory_bus) (M) (Axi_config) (Axi)

  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; write_to_controller : 'a Write_bus.Source.t list [@length M.num_write_channels]
      ; read_to_controller : 'a Read_bus.Source.t list [@length M.num_read_channels]
      ; axi : 'a Axi.I.t [@rtlprefix "axi_i_"]
      }
    [@@deriving hardcaml ~rtlmangle:"$"]
  end

  module O = struct
    type 'a t =
      { write_to_controller : 'a Write_bus.Dest.t list [@length M.num_write_channels]
      ; read_to_controller : 'a Read_bus.Dest.t list [@length M.num_read_channels]
      ; write_response : 'a Write_response.With_valid.t list
            [@length M.num_write_channels]
      ; read_response : 'a Read_response.With_valid.t list [@length M.num_read_channels]
      ; axi : 'a Axi.O.t [@rtlprefix "axi_o_"]
      }
    [@@deriving hardcaml ~rtlmangle:"$"]
  end

  let create
        ~request_delay
        ~priority_mode
        scope
        ({ clock; clear; write_to_controller; read_to_controller; axi } : _ I.t)
    =
    let reg_spec_no_clear = Reg_spec.create ~clock () in
    let write_arbitrator =
      Write_arbitrator.hierarchical
        ~instance:"write"
        ~priority_mode
        scope
        { Write_arbitrator.I.clock; clear; ch_to_controller = write_to_controller }
    in
    let read_arbitrator =
      Read_arbitrator.hierarchical
        ~instance:"read"
        ~priority_mode
        scope
        { Read_arbitrator.I.clock; clear; ch_to_controller = read_to_controller }
    in
    let core =
      Core.hierarchical
        scope
        { Core.I.clock
        ; clear
        ; which_write_ch =
            pipeline ~n:request_delay reg_spec_no_clear write_arbitrator.which_ch
        ; selected_write_ch =
            Write_bus.Source.Of_signal.pipeline
              ~n:request_delay
              reg_spec_no_clear
              write_arbitrator.selected_ch
        ; which_read_ch =
            pipeline ~n:request_delay reg_spec_no_clear read_arbitrator.which_ch
        ; selected_read_ch =
            Read_bus.Source.Of_signal.pipeline
              ~n:request_delay
              reg_spec_no_clear
              read_arbitrator.selected_ch
        ; axi
        }
    in
    (* TODO: Propagate errors *)
    { O.write_to_controller =
        List.map
          ~f:(fun t -> { Write_bus.Dest.ready = t.ready &: core.write_ready })
          write_arbitrator.acks
    ; read_to_controller =
        List.map
          ~f:(fun t -> { Read_bus.Dest.ready = t.ready &: core.read_ready })
          read_arbitrator.acks
    ; write_response = core.write_response
    ; read_response = core.read_response
    ; axi = core.axi
    }
  ;;

  let hierarchical ~request_delay ~priority_mode (scope : Scope.t) (input : Signal.t I.t) =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical
      ~scope
      ~name:"memory_controller"
      (create ~priority_mode ~request_delay)
      input
  ;;
end
