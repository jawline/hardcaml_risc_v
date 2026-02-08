open! Core
open Hardcaml
open Signal

module Make (Hart_config : Hart_config_intf.S) = struct
  module Op = Op.Make (Hart_config)

  let register_width = Register_width.bits Hart_config.register_width

  module I = struct
    type 'a t =
      { alu_op : 'a Alu_operation.Onehot.t
      ; muldiv_op : 'a Funct3.Muldiv.Onehot.t
      ; is_muldiv : 'a
      ; lhs : 'a [@bits register_width]
      ; rhs : 'a [@bits register_width]
      }
    [@@deriving hardcaml ~rtlmangle:"$"]
  end

  module O = struct
    type 'a t = { rd : 'a [@bits register_width] } [@@deriving hardcaml ~rtlmangle:"$"]
  end

  let multiply_result scope (i : _ I.t) =
    let op = i.muldiv_op in
    let%hw lhs =
      Funct3.Muldiv.Onehot.switch
        ~f:(function
          | Funct3.Muldiv.Mul | MulHigh | MulHigh_Signed_Unsigned ->
            sextend ~width:(register_width * 2) i.lhs
          | MulHigh_Unsigned -> uextend ~width:(register_width * 2) i.lhs)
        op
    in
    let%hw rhs =
      Funct3.Muldiv.Onehot.switch
        ~f:(function
          | Funct3.Muldiv.Mul | MulHigh -> sextend ~width:(register_width * 2) i.rhs
          | MulHigh_Signed_Unsigned | MulHigh_Unsigned ->
            uextend ~width:(register_width * 2) i.rhs)
        op
    in
    let%hw full_width_result = sel_bottom ~width:(register_width * 2) (lhs *+ rhs) in
    let%hw result =
      Funct3.Muldiv.Onehot.switch
        ~f:(function
          | Funct3.Muldiv.Mul -> sel_bottom ~width:register_width full_width_result
          | MulHigh | MulHigh_Signed_Unsigned | MulHigh_Unsigned ->
            sel_top ~width:register_width full_width_result)
        op
    in
    result
  ;;

  let create scope (i : _ I.t) =
    let op = Op.hierarchical scope { Op.I.op = i.alu_op; lhs = i.lhs; rhs = i.rhs } in
    let rd = mux2 i.is_muldiv (multiply_result scope i) op.rd in
    { O.rd }
  ;;

  let hierarchical (scope : Scope.t) (input : Signal.t I.t) =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ~scope ~name:"op_with_mul" create input
  ;;
end
