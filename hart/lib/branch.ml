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
      ; funct3 : 'a Funct3.Branch.Onehot.t
      ; branch_offset : 'a [@bits register_width]
      }
    [@@deriving hardcaml ~rtlmangle:"$"]
  end

  module O = struct
    type 'a t = { new_pc : 'a [@bits register_width] }
    [@@deriving hardcaml ~rtlmangle:"$"]
  end

  let create (_scope : Scope.t) ({ I.pc; funct3; lhs; rhs; branch_offset } : _ I.t) =
    let branch_when ~key ~f =
      [ { With_valid.valid = Funct3.Branch.Onehot.valid funct3 key &: f lhs rhs
        ; value = pc +: branch_offset
        }
      ; { With_valid.valid = Funct3.Branch.Onehot.valid funct3 key &: ~:(f lhs rhs)
        ; value = pc +:. 4
        }
      ]
    in
    let all_options_with_valid =
      List.concat
        [ branch_when ~key:Beq ~f:( ==: )
        ; branch_when ~key:Bne ~f:( <>: )
        ; branch_when ~key:Blt ~f:( <+ )
        ; branch_when ~key:Bge ~f:( >=+ )
        ; branch_when ~key:Bltu ~f:( <: )
        ; branch_when ~key:Bgeu ~f:( >=: )
        ]
    in
    { O.new_pc = onehot_select all_options_with_valid }
  ;;

  let hierarchical ~instance (scope : Scope.t) (input : Signal.t I.t) =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ~scope ~name:"branch" ~instance create input
  ;;
end
