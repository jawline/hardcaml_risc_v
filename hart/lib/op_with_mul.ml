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

  module Prefix_multiplier = Prefix_multiplier.Make (struct
      let width = register_width * 2
    end)

  let partial_product_and_accumulate ~lhs ~rhs scope (i : _ I.t) =
    let spec = Clocking.to_spec i.clock in
    let spec_no_clear = Clocking.to_spec_no_clear i.clock in
    let%hw valid = reg spec (i.valid &: i.is_muldiv) in
    let%hw lhs = reg spec_no_clear lhs in
    let%hw rhs = reg spec_no_clear rhs in
    let%hw.Funct3.Muldiv.Onehot.Of_signal cached_op =
      Funct3.Muldiv.Onehot.Of_signal.reg ~enable:valid spec_no_clear i.muldiv_op
    in
    let o =
      Prefix_multiplier.hierarchical
        scope
        { Prefix_multiplier.I.clock = i.clock; valid; lhs; rhs }
    in
    { O.valid = o.valid
    ; rd =
        Funct3.Muldiv.Onehot.switch
          ~f:(function
            | Funct3.Muldiv.Mul -> sel_bottom ~width:register_width o.value
            | MulHigh | MulHigh_Signed_Unsigned | MulHigh_Unsigned ->
              sel_top ~width:register_width o.value)
          cached_op
    }
  ;;

  let multiply_result scope (i : _ I.t) =
    let op = i.muldiv_op in
    let%hw lhs_extended =
      Funct3.Muldiv.Onehot.switch
        ~f:(function
          | Funct3.Muldiv.Mul | MulHigh | MulHigh_Signed_Unsigned ->
            sextend ~width:(register_width * 2) i.lhs
          | MulHigh_Unsigned -> uextend ~width:(register_width * 2) i.lhs)
        op
    in
    let%hw rhs_extended =
      Funct3.Muldiv.Onehot.switch
        ~f:(function
          | Funct3.Muldiv.Mul | MulHigh -> sextend ~width:(register_width * 2) i.rhs
          | MulHigh_Signed_Unsigned | MulHigh_Unsigned ->
            uextend ~width:(register_width * 2) i.rhs)
        op
    in
    partial_product_and_accumulate ~lhs:lhs_extended ~rhs:rhs_extended scope i
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
    { O.valid = op.valid |: multiply_result.valid
    ; rd = mux2 multiply_result.valid multiply_result.rd op.rd
    }
  ;;

  let hierarchical (scope : Scope.t) (input : Signal.t I.t) =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ~scope ~name:"op_with_mul" create input
  ;;
end
