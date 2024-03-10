open! Core
open Hardcaml
open Signal
open Always

module Make
    (Hart_config : Hart_config_intf.S)
    (Memory_config : sig
       val num_bytes : int
     end)
    (General_config : sig
       val num_harts : int
     end) =
struct
  module Memory_controller = Memory_controller.Make (struct
      let num_bytes = Memory_config.num_bytes
      let num_channels = General_config.num_harts
      let address_width = Address_width.bits Hart_config.address_width
      let data_bus_width = 32
    end)

  module Hart = Hart.Make (Hart_config) (Memory_controller)

  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t =
      { registers : 'a Hart.Registers.t list [@length General_config.num_harts] }
    [@@deriving sexp_of, hardcaml]
  end

  let create scope (i : _ I.t) =
    let ch_to_controller_per_hart =
      List.init
        ~f:(fun _which_hart -> Memory_controller.Tx_bus.Tx.Of_always.wire zero)
        General_config.num_harts
    in
    let controller_to_ch_per_hart =
      List.init
        ~f:(fun _which_hart -> Memory_controller.Rx_bus.Rx.Of_always.wire zero)
        General_config.num_harts
    in
    let controller =
      Memory_controller.hierarchical
        ~instance:"Memory_controller"
        scope
        { Memory_controller.I.clock = i.clock
        ; clear = i.clear
        ; ch_to_controller =
            List.map
              ~f:Memory_controller.Tx_bus.Tx.Of_always.value
              ch_to_controller_per_hart
        ; controller_to_ch =
            List.map
              ~f:Memory_controller.Rx_bus.Rx.Of_always.value
              controller_to_ch_per_hart
        }
    in
    let harts =
      List.init
        ~f:(fun which_hart ->
          Hart.hierarchical
            ~instance:[%string "hart_%{which_hart#Int}"]
            scope
            { Hart.I.clock = i.clock
            ; clear = i.clear
            ; memory_controller_to_hart =
                List.nth_exn controller.controller_to_ch which_hart
            ; hart_to_memory_controller =
                List.nth_exn controller.ch_to_controller which_hart
            })
        General_config.num_harts
    in
    compile
      [ List.map
          ~f:(fun (hart, ch_to_controller_per_hart) ->
            Memory_controller.Tx_bus.Tx.Of_always.assign
              ch_to_controller_per_hart
              hart.hart_to_memory_controller)
          (List.zip_exn harts ch_to_controller_per_hart)
        |> proc
      ; List.map
          ~f:(fun (hart, controller_to_ch_per_hart) ->
            Memory_controller.Rx_bus.Rx.Of_always.assign
              controller_to_ch_per_hart
              hart.memory_controller_to_hart)
          (List.zip_exn harts controller_to_ch_per_hart)
        |> proc
      ];
    { O.registers = List.map ~f:(fun o -> o.registers) harts }
  ;;

  let hierarchical ~instance (scope : Scope.t) (input : Signal.t I.t) =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ~scope ~name:"Cpu" ~instance create input
  ;;
end
