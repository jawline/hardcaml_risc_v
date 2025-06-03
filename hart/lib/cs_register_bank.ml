open! Core
open Hardcaml
open Signal

(* TODO: Test *)

module type Bank_config = sig
  val address_bits : int
  val register_io : valid:t -> address:t -> also_write:t -> value:t -> t With_valid.t
end

module Make (Hart_config : Hart_config_intf.S) (Bank_config : Bank_config) = struct
  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; enable : 'a
      ; is_write : 'a
      ; write_value : 'a [@bits Register_width.bits Hart_config.register_width]
      ; address : 'a [@bits Bank_config.address_bits]
      }
    [@@deriving hardcaml ~rtlmangle:"$"]
  end

  module O = struct
    type 'a t =
      { valid : 'a
      ; value : 'a [@bits Register_width.bits Hart_config.register_width]
      }
    [@@deriving hardcaml ~rtlmangle:"$"]
  end

  let create _scope { I.clock = _; clear = _; enable; is_write; address; write_value } =
    let result =
      Bank_config.register_io
        ~valid:enable
        ~address
        ~also_write:is_write
        ~value:write_value
    in
    { O.valid = result.valid; value = result.value }
  ;;

  let hierarchical (scope : Scope.t) (input : Signal.t I.t) =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ~scope ~name:"cs_register_bank" create input
  ;;
end
