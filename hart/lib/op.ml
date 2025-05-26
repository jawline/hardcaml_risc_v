open! Core
open Hardcaml
open Signal

module Make (Hart_config : Hart_config_intf.S) = struct
  let register_width = Register_width.bits Hart_config.register_width

  module I = struct
    type 'a t =
      { funct3 : 'a Funct3.Op.Onehot.t
      ; subtract_instead_of_add : 'a
      ; arithmetic_shift : 'a
      ; lhs : 'a [@bits register_width]
      ; rhs : 'a [@bits register_width]
      }
    [@@deriving hardcaml ~rtlmangle:"$"]
  end

  module O = struct
    type 'a t = { rd : 'a [@bits register_width] } [@@deriving hardcaml ~rtlmangle:"$"]
  end

  let create
        _scope
        ({ I.funct3; subtract_instead_of_add; arithmetic_shift; lhs; rhs } : _ I.t)
    =
    let rd =
      Funct3.Op.Onehot.switch
        ~f:(function
          | Funct3.Op.Add_or_sub -> mux2 subtract_instead_of_add (lhs -: rhs) (lhs +: rhs)
          | Slt -> uresize ~width:32 (lhs <+ rhs)
          | Sltu -> uresize ~width:32 (lhs <: rhs)
          | Sll -> log_shift ~f:sll ~by:rhs lhs
          | Xor -> lhs ^: rhs
          | Or -> lhs |: rhs
          | And -> lhs &: rhs
          | Srl_or_sra ->
            let sra = log_shift ~f:sra ~by:(sel_bottom ~width:6 rhs) lhs in
            let srl = log_shift ~f:srl ~by:(sel_bottom ~width:6 rhs) lhs in
            mux2 arithmetic_shift sra srl)
        funct3
    in
    { O.rd }
  ;;

  let hierarchical ~instance (scope : Scope.t) (input : Signal.t I.t) =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ~scope ~name:"op" ~instance create input
  ;;
end
