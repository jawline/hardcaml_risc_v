open! Core
open Hardcaml
open Signal

module type Bank_config = sig
        val address_bits : int
        val register_write : valid:t -> address:t -> value:t -> t With_valid.t
        val register_read : valid:t -> address:t -> t With_valid.t
end

(** This forms a generic bank of registers we can use to implement the CS
    (control & status) registers. These expose information about the system
    environment to the running software (e.g., clock cycles, time since
    startup). The CS registers are formed out of a set of banks which have
    contiguous register allocations. Register address are register width
    aligned. *)
module Make
    (Hart_config : Hart_config_intf.S) (Bank_config : Bank_config) =
struct

        module I = struct
                type 'a t = {
                        clock : 'a 
                        ; clear : 'a 
                        ; enable : 'a (** Enable should be pulsed rather than held high. Each pulse indicates a unique read / write. The caller should expect a response, but it may take many cycles. *)
                        ; is_write : 'a 
                        ; write_value : 'a [@bits (assert false)]
                        ; address : 'a [@bits Bank_config.address_bits]
                } [@@deriving hardcaml ~rtlmangle:"$"]
        end


        module O = struct

                type 'a t = { valid : 'a ; data : 'a [@bits (assert false)] }  [@@deriving hardcaml ~rtlmangle:"$"]
        end


  let create scope { I.clock ; clear = _ ; enable ; is_write ; address ; write_value } = 
    

      let read_result = Bank_config.register_read ~valid:(enable &: ~:(is_write))
          ~address in

      let write_result = Bank_config.register_write ~valid:(enable &: is_write)
      ~address ~value:write_value in 


      (* TODO: Does it make sense to combine read and write valids? In principle, if the system puts many in flight they could come back on the same cycle, but I think this design is already unsound if we put many in flight and would need some transaction ID. *)

      { O.valid = read_result.valid |: write_result.valid 
      ; data = read_result.value }

          



  let hierarchical (scope : Scope.t) (input : Signal.t I.t) =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ~scope ~name:"cs_register_bank" create input
  ;;
end
