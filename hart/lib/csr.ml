open! Core
open Hardcaml
open Signal

module Make
    (Hart_config : Hart_config_intf.S)
    (Registers : Registers_intf.S)
    (Decoded_instruction : Decoded_instruction_intf.M(Registers).S) =
struct
  module Cs_registers = Cs_registers.Make (Hart_config)

  let register_width = Register_width.bits Hart_config.register_width

  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; valid : 'a
      ; instruction : 'a Decoded_instruction.t
      ; instret : 'a
      }
    [@@deriving hardcaml ~rtlmangle:"$"]
  end

  module O = struct
    type 'a t =
      { valid : 'a
      ; value : 'a [@bits register_width]
      }
    [@@deriving hardcaml ~rtlmangle:"$"]
  end

  let create scope ({ clock; clear; valid; instruction; instret } : _ I.t) =
    (* TODO: Tests *)
    let csrrw = instruction.funct3 ==:. Funct3.System.to_int Csrrw in
    let csrrs = instruction.funct3 ==:. Funct3.System.to_int Csrrs in
    let csrrc = instruction.funct3 ==:. Funct3.System.to_int Csrrc in
    (* TODO: Send a zero / sign extend signal too so the bank can decide how to expand bank signals smaller than the Hart register with. *)
    let register_io =
      Cs_registers.hierarchical
        ~clock_frequency:Hart_config.design_frequency
        scope
        { Cs_registers.I.clock
        ; clear
        ; enable = valid &: (csrrw |: csrrs |: csrrc)
        ; is_write = csrrw
        ; write_value = instruction.rs1
        ; address = instruction.csr
        ; instret
        }
    in
    { O.valid = register_io.valid; value = register_io.value }
  ;;

  let hierarchical (scope : Scope.t) (input : Signal.t I.t) =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ~scope ~name:"csr" create input
  ;;
end
