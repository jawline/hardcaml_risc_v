open! Core
open Hardcaml
open Signal

module Make (Hart_config : Hart_config_intf.S) = struct
  let register_width = Register_width.bits Hart_config.register_width

  module I = struct
    type 'a t =
      { funct3 : 'a [@bits 3]
      ; funct7_switch : 'a
      ; funct7_error : 'a
      ; lhs : 'a [@bits register_width]
      ; rhs : 'a [@bits register_width]
      }
    [@@deriving hardcaml ~rtlmangle:"$"]
  end

  module O = struct
    type 'a t =
      { rd : 'a [@bits register_width] [@rtlname "new_rd"]
      ; error : 'a
      }
    [@@deriving hardcaml ~rtlmangle:"$"]
  end

  let create
    ~enable_subtract
    _scope
    ({ I.funct3; funct7_switch; funct7_error; lhs; rhs } : _ I.t)
    =
    let rd, error =
      Util.switch2
        (module Funct3.Op)
        ~if_not_found:(zero register_width, vdd)
        ~f:(function
          | Funct3.Op.Add_or_sub ->
            if enable_subtract
            then mux2 funct7_switch (lhs -: rhs) (lhs +: rhs), funct7_error
            else lhs +: rhs, gnd
          | Slt -> uresize ~width:32 (lhs <+ rhs), gnd
          | Sltu -> uresize ~width:32 (lhs <: rhs), gnd
          | Sll -> log_shift ~f:sll ~by:rhs lhs, gnd
          | Xor -> lhs ^: rhs, gnd
          | Or -> lhs |: rhs, gnd
          | And -> lhs &: rhs, gnd
          | Srl_or_sra ->
            let sra = log_shift ~f:sra ~by:rhs lhs in
            let srl = log_shift ~f:srl ~by:rhs lhs in
            mux2 funct7_switch sra srl, funct7_error)
        funct3
    in
    { O.rd; error }
  ;;

  let hierarchical ~enable_subtract ~instance (scope : Scope.t) (input : Signal.t I.t) =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ~scope ~name:"op" ~instance (create ~enable_subtract) input
  ;;
end
