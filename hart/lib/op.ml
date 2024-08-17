open! Core
open Hardcaml
open Signal

module Make (Hart_config : Hart_config_intf.S) = struct
  let register_width = Register_width.bits Hart_config.register_width

  module I = struct
    type 'a t =
      { funct3 : 'a [@bits 3]
      ; funct7 : 'a [@bits 7]
      ; lhs : 'a [@bits register_width]
      ; rhs : 'a [@bits register_width]
      }
    [@@deriving sexp_of, hardcaml ~rtlmangle:"$"]
  end

  module O = struct
    type 'a t =
      { rd : 'a [@bits register_width] [@rtlname "new_rd"]
      ; error : 'a
      }
    [@@deriving sexp_of, hardcaml ~rtlmangle:"$"]
  end

  let create ~enable_subtract scope ({ I.funct3; funct7; lhs; rhs } : _ I.t) =
    let rd, error =
      Util.switch2
        (module Funct3.Op)
        ~if_not_found:(zero register_width, vdd)
        ~f:(function
          | Funct3.Op.Add_or_sub ->
            if enable_subtract
            then (
              let error = funct7 >:. 1 in
              let%hw is_subtract = funct7.:(5) in
              mux2 is_subtract (lhs -: rhs) (lhs +: rhs), error)
            else lhs +: rhs, gnd
          | Slt -> uresize ~width:32 (lhs <+ rhs), gnd
          | Sltu -> uresize ~width:32 (lhs <: rhs), gnd
          | Sll -> Util.sll lhs rhs, gnd
          | Xor -> lhs ^: rhs, gnd
          | Or -> lhs |: rhs, gnd
          | And -> lhs &: rhs, gnd
          | Srl_or_sra ->
            let error = funct7 >:. 1 in
            (* TODO: Not sure if this is correct for SRA *)
            let sra = Util.sra lhs rhs in
            let srl = Util.srl lhs rhs in
            mux2 funct7.:(0) sra srl, error)
        funct3
    in
    { O.rd; error }
  ;;

  let hierarchical ~enable_subtract ~instance (scope : Scope.t) (input : Signal.t I.t) =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ~scope ~name:"op" ~instance (create ~enable_subtract) input
  ;;
end
