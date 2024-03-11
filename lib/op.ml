open! Core
open Hardcaml
open Signal

module Make
    (Hart_config : Hart_config_intf.S)
    (Memory : Memory_bus_intf.S)
    (Registers : Registers_intf.S) =
struct
  module I = struct
    type 'a t =
      { funct3 : 'a [@bits 3]
      ; funct7 : 'a [@bits 7]
      ; rs1 : 'a [@bits Register_width.bits Hart_config.register_width]
      ; rs2 : 'a [@bits Register_width.bits Hart_config.register_width]
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t =
      { rd : 'a [@bits Register_width.bits Hart_config.register_width]
      ; error : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  let create _scope ({ funct3; funct7; rs1; rs2 } : _ I.t) =
    let operations_and_errors =
      List.init
        ~f:(fun funct3 ->
          match Funct3.Op.of_int_exn funct3 with
          | Funct3.Op.Add_or_sub ->
            let error = funct7 >=:. 1 in
            mux2 (select funct7 0 0) (rs1 -: rs2) (rs1 +: rs2), error
          | Sll ->
            (* TODO: log_shift by > 32 is always zero, it might be better to special case that. *)
            log_shift sll rs1 rs2, zero 1
          | _ ->
            print_s [%message "BUG: Unimplemented"];
            of_int ~width:32 0, zero 1)
        8
    in
    let operations, errors = List.unzip operations_and_errors in
    { O.rd = mux funct3 operations; error = mux funct3 errors }
  ;;

  let hierarchical ~instance (scope : Scope.t) (input : Signal.t I.t) =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ~scope ~name:"Op" ~instance create input
  ;;
end
