open! Core
open Hardcaml
open! Signal

module Make (Hart_config : Hart_config_intf.S) (Memory : Memory_bus_intf.S) = struct
  let register_width = Register_width.bits Hart_config.register_width

  module I = struct
    type 'a t =
      { funct3 : 'a [@bits 3]
      ; source : 'a [@bits register_width]
      ; memory_controller_to_hart : 'a Memory.Rx_bus.Tx.t
           [@rtlprefix "memory_controller_to_hart"]
      ; hart_to_memory_controller : 'a Memory.Tx_bus.Rx.t
           [@rtlprefix "hart_to_memory_controller"]
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t =
      { new_rd : 'a [@bits register_width] [@rtlname "new_rd"]
      ; error : 'a
      ; finished : 'a
      ; memory_controller_to_hart : 'a Memory.Rx_bus.Rx.t
           [@rtlprefix "memory_controller_to_hart"]
      ; hart_to_memory_controller : 'a Memory.Tx_bus.Tx.t
           [@rtlprefix "hart_to_memory_controller"]
      }
    [@@deriving sexp_of, hardcaml]
  end

  let create
    (_scope : Scope.t)
    ({ I.funct3 = _
     ; source = _
     ; memory_controller_to_hart = _
     ; hart_to_memory_controller = _
     } :
      _ I.t)
    =
    assert false
  ;;

  let hierarchical ~instance (scope : Scope.t) (input : Signal.t I.t) =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ~scope ~name:"Load" ~instance create input
  ;;
end
