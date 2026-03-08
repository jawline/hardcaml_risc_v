open! Core
open Hardcaml
open Signal

module Make (Hart_config : Hart_config_intf.S) = struct
  module Op = Op.Make (Hart_config)

  let register_width = Register_width.bits Hart_config.register_width

  module I = struct
    type 'a t =
      { clock : 'a Clocking.t
      ; valid : 'a
      ; alu_op : 'a Alu_operation.Onehot.t
      ; muldiv_op : 'a Funct3.Muldiv.Onehot.t
      ; is_muldiv : 'a
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
    { O.valid = pipeline ~n:4 (Clocking.to_spec i.clock) (i.valid &: i.is_muldiv)
    ; rd = pipeline ~n:4 (Clocking.to_spec_no_clear i.clock) result
    }
  ;;

  let create scope (i : _ I.t) =
    let op =
      Op.hierarchical
        scope
        { Op.I.valid = i.valid &: ~:(i.is_muldiv)
        ; op = i.alu_op
        ; lhs = i.lhs
        ; rhs = i.rhs
        }
    in
    let multiply_result = multiply_result scope i in
    { O.valid = op.valid |: multiply_result.valid; rd = op.rd |: multiply_result.rd }
  ;;

  let hierarchical (scope : Scope.t) (input : Signal.t I.t) =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ~scope ~name:"op_with_mul" create input
  ;;
end
