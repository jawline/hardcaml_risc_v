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

  (* TODO: Op and op imm are the same except rs2 is swapped out for
   * i_immediate. Just parameterize the instance instead. *)

  let create _scope ({ funct3; funct7; rs1; rs2; _ } : _ Decoded_instruction.t) =
    let operations_and_errors =
      List.init
        ~f:(fun funct3 ->
          match Funct3.Op.of_int_exn funct3 with
          | Funct3.Op.Add_or_sub ->
            let error = funct7 >=:. 1 in
            mux2 (select funct7 0 0) (rs1 -: rs2) (rs1 +: rs2), error
          | Slt -> uresize (rs1 <+ rs2) 32, zero 1
          | Sltu -> uresize (rs1 <: rs2) 32, zero 1
          | Sll ->
            (* If > 32, set the register to zero. *)
            (* TODO: This is very slow, consider just a LUT instead. *)
            let rd = Util.sll rs1 rs2 in
            rd, zero 1
          | Xor -> rs1 ^: rs2, zero 1
          | Or -> rs1 |: rs2, zero 1
          | And -> rs1 &: rs2, zero 1
          | Srl_or_sra ->
            let error = funct7 >=:. 1 in
            (* TODO: Not sure if this is correct for SRA *)
            let sra = Util.sra rs1 rs2 in
            let srl = Util.srl rs1 rs2 in
            mux2 (select funct7 0 0) sra srl, error)
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
