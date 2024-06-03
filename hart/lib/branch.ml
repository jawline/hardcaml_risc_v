open! Core
open Hardcaml
open Signal

module Make (Hart_config : Hart_config_intf.S) = struct
  let register_width = Register_width.bits Hart_config.register_width

  module I = struct
    type 'a t =
      { pc : 'a [@bits register_width]
      ; lhs : 'a [@bits register_width]
      ; rhs : 'a [@bits register_width]
      ; funct3 : 'a [@bits 3]
      ; b_immediate : 'a [@bits register_width]
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t =
      { new_pc : 'a [@bits register_width] [@rtlname "new_rd"]
      ; error : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  let create (_scope : Scope.t) ({ I.pc; funct3; lhs; rhs; b_immediate } : _ I.t) =
    let branch_when ~f = mux2 (f lhs rhs) (pc +: b_immediate) (pc +:. 4), zero 1 in
    let new_pc, error =
      Util.switch2
        (module Funct3.Branch)
        ~if_not_found:(zero register_width, vdd)
        ~f:(function
          | Funct3.Branch.Beq -> branch_when ~f:( ==: )
          | Bne -> branch_when ~f:( <>: )
          | Blt -> branch_when ~f:( <+ )
          | Bge -> branch_when ~f:( >=+ )
          | Bltu -> branch_when ~f:( <: )
          | Bgeu -> branch_when ~f:( >=: ))
        funct3
    in
    { O.new_pc; error }
  ;;

  let hierarchical ~instance (scope : Scope.t) (input : Signal.t I.t) =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ~scope ~name:"Branch" ~instance create input
  ;;
end
