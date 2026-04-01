open! Core
open Hardcaml
open Signal

module Make (Hart_config : Hart_config_intf.S) = struct
  let register_width = Register_width.bits Hart_config.register_width

  module I = struct
    type 'a t =
      { valid : 'a
      ; op : 'a Alu_operation.Onehot.t
      ; lhs : 'a [@bits register_width]
      ; rhs : 'a [@bits register_width]
      }
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t =
      { valid : 'a [@rtlprefix "output_"]
      ; rd : 'a [@bits register_width]
      }
    [@@deriving hardcaml]
  end

  let () =
    if Hart_config.Extensions.zba && register_width <> 32
    then raise_s [%message "BUG: Op does not support all 64 bit ZBA instructions."]
  ;;

  let support_if_zba result =
    if Hart_config.Extensions.zba then result else zero register_width
  ;;

  let sh_add ~by lhs rhs = rhs +: sll ~by lhs |> support_if_zba

  let create _scope ({ I.valid; op; lhs; rhs } : _ I.t) =
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
          | Sra -> log_shift ~f:sra ~by:(sel_bottom ~width:6 rhs) lhs
          | Sh1_add -> sh_add ~by:1 lhs rhs
          | Sh2_add -> sh_add ~by:2 lhs rhs
          | Sh3_add -> sh_add ~by:3 lhs rhs)
        op
    in
    { O.valid; rd }
  ;;

  let hierarchical (scope : Scope.t) (input : Signal.t I.t) =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ~scope ~name:"op" create input
  ;;
end
