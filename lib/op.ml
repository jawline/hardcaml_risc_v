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
    [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t =
      { rd : 'a [@bits register_width] [@rtlname "new_rd"]
      ; error : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  let create _scope ({ I.funct3; funct7; lhs; rhs } : _ I.t) =
    let rd, error =
      Util.switch2
        (module Funct3.Op)
        ~if_not_found:(zero register_width, one 1)
        ~f:(function
          | Funct3.Op.Add_or_sub ->
            let error = funct7 >:. 1 in
            mux2 (select funct7 0 0) (lhs -: rhs) (lhs +: rhs), error
          | Slt -> uresize (lhs <+ rhs) 32, zero 1
          | Sltu -> uresize (lhs <: rhs) 32, zero 1
          | Sll ->
            (* If > 32, set the register to zero. *)
            (* TODO: This is very slow, consider just a LUT instead. *)
            let rd = Util.sll lhs rhs in
            rd, zero 1
          | Xor -> lhs ^: rhs, zero 1
          | Or -> lhs |: rhs, zero 1
          | And -> lhs &: rhs, zero 1
          | Srl_or_sra ->
            let error = funct7 >:. 1 in
            (* TODO: Not sure if this is correct for SRA *)
            let sra = Util.sra lhs rhs in
            let srl = Util.srl lhs rhs in
            mux2 (select funct7 0 0) sra srl, error)
        funct3
    in
    { O.rd; error }
  ;;

  let hierarchical ~instance (scope : Scope.t) (input : Signal.t I.t) =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ~scope ~name:"Op" ~instance create input
  ;;
end
