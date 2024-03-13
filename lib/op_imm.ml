open! Core
open Hardcaml
open Signal

module Make
    (Hart_config : Hart_config_intf.S)
    (Memory : Memory_bus_intf.S)
    (Decoded_instruction : Decoded_instruction_intf.S) =
struct
  module I = Decoded_instruction

  module O = struct
    type 'a t =
      { rd : 'a [@bits Register_width.bits Hart_config.register_width] [@rtlname "new_rd"]
      ; error : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  let create _scope ({ funct3; rs1; i_immediate; _ } : _ Decoded_instruction.t) =
    let operations_and_errors =
      List.init
        ~f:(fun funct3 ->
          match Funct3.Op_imm.of_int_exn funct3 with
          | Funct3.Op_imm.Addi -> rs1 +: i_immediate, zero 1
          | Slti -> uresize (rs1 <+ i_immediate) 32, zero 1
          | Sltiu -> uresize (rs1 <: i_immediate) 32 , zero 1
          | Andi -> rs1 &: i_immediate, zero 1
          | Ori -> rs1 |: i_immediate, zero 1
          | Xori -> rs1 ^: i_immediate, zero 1
          | Slli ->
            print_s [%message "BUG: TODO"];
            zero 32, zero 1
          | Srli_or_srai ->
            print_s [%message "BUG: TODO"];
            zero 32, zero 1)
        8
    in
    let operations, errors = List.unzip operations_and_errors in
    { O.rd = mux funct3 operations; error = mux funct3 errors }
  ;;

  let hierarchical ~instance (scope : Scope.t) (input : Signal.t I.t) =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ~scope ~name:"Op_imm" ~instance create input
  ;;
end
