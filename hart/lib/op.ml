open! Core
open Hardcaml
open Signal

module Make (Hart_config : Hart_config_intf.S) = struct
  let register_width = Register_width.bits Hart_config.register_width

  module I = struct
    type 'a t =
      { op : 'a Alu_operation.Onehot.t
      ; lhs : 'a [@bits register_width]
      ; rhs : 'a [@bits register_width]
      }
    [@@deriving hardcaml ~rtlmangle:"$"]
  end

  module O = struct
    type 'a t = { rd : 'a [@bits register_width] } [@@deriving hardcaml ~rtlmangle:"$"]
  end

  let create _scope ({ I.op; lhs; rhs } : _ I.t) =
    let rd =
      Alu_operation.Onehot.switch
        ~f:(function
          | Alu_operation.Add -> lhs +: rhs
          | Sub -> lhs -: rhs
          | Slt -> uresize ~width:32 (lhs <+ rhs)
          | Sltu -> uresize ~width:32 (lhs <: rhs)
          | Sll -> log_shift ~f:sll ~by:(sel_bottom ~width:6 rhs) lhs
          | Xor -> lhs ^: rhs
          | Or -> lhs |: rhs
          | And -> lhs &: rhs
          | Srl -> log_shift ~f:srl ~by:(sel_bottom ~width:6 rhs) lhs
          | Sra -> log_shift ~f:sra ~by:(sel_bottom ~width:6 rhs) lhs)
        op
    in
    { O.rd }
  ;;

  let hierarchical (scope : Scope.t) (input : Signal.t I.t) =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ~scope ~name:"op" create input
  ;;
end
