open! Core
open Hardcaml
open Signal

(* TODO: Test *)

(** This forms the control & status register set. The control & status registers
    are a memory region, addressed in register widths, which software can
    use to read from the device state or change device state.

    The registers are split into small contiguous banks, each having a specific
    purpose. The banks are not necessarily contiguous. To implement this
    we activate a bank based on the upper bits of an address and then mux the result
    based on the same upper bits. The lower bits are used as the bank address.*)
module Make (Hart_config : Hart_config_intf.S) = struct
  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; enable : 'a
      ; is_write : 'a
      ; address : 'a [@bits 12]
      ; write_value : 'a [@bits Register_width.bits Hart_config.register_width]
      ; instret : 'a
      }
    [@@deriving hardcaml ~rtlmangle:"$"]
  end

  module O = struct
    type 'a t =
      { valid : 'a
      ; value : 'a [@bits Register_width.bits Hart_config.register_width]
      }
    [@@deriving hardcaml ~rtlmangle:"$"]
  end

  let create
    ~clock_frequency
    scope
    { I.clock; clear; instret; enable; is_write; address; write_value }
    =
    let spec = Reg_spec.create ~clock ~clear () in
    let cycle = reg_fb spec ~width:64 ~f:(fun t -> t +:. 1) in
    let ns_per_cycle =
      let ns_per_second = 1_000_000_000 in
      if ns_per_second % clock_frequency <> 0
      then
        raise_s
          [%message
            "BUG: The clock incrementer does not support non-integer ns per second \
             values."
              (clock_frequency : int)
              (ns_per_second / clock_frequency : int)
              (ns_per_second % clock_frequency : int)];
      ns_per_second / clock_frequency
    in
    let time =
      (* Increment on cycle by ns per cycle *)
      reg_fb spec ~width:64 ~f:(fun t -> t +:. ns_per_cycle)
    in
    let instret = reg_fb spec ~enable:instret ~width:64 ~f:(fun t -> t +:. 1) in
    let module Unpriveleged_counters_and_timers =
      Cs_register_bank.Make
        (Hart_config)
        (struct
          let address_bits = 10

          (* These registers are only defined in 32b processor variants, as
             the register reads are not sufficient for the whole register. *)
          let upper_registers =
            match Hart_config.register_width with
            | B32 ->
              [ { With_valid.valid = address ==:. 0x80
                ; value =
                    sel_top ~width:(Register_width.bits Hart_config.register_width) cycle
                }
              ; { valid = address ==:. 0x81
                ; value =
                    sel_top ~width:(Register_width.bits Hart_config.register_width) time
                }
              ; { valid = address ==:. 0x82
                ; value =
                    sel_top
                      ~width:(Register_width.bits Hart_config.register_width)
                      instret
                }
              ]
          ;;

          let register_io ~valid ~address ~also_write:_ ~value:_ =
            (* TODO: Indicate errors or fault when reading from bad address. *)
            (* Writes are ignored in this register bank. *)
            { With_valid.valid = reg spec valid
            ; value =
                reg
                  spec
                  (onehot_select
                     ([ { With_valid.valid = address ==:. 0x0
                        ; value =
                            sel_bottom
                              ~width:(Register_width.bits Hart_config.register_width)
                              cycle
                        }
                      ; { valid = address ==:. 0x1
                        ; value =
                            sel_bottom
                              ~width:(Register_width.bits Hart_config.register_width)
                              time
                        }
                      ; { valid = address ==:. 0x2
                        ; value =
                            sel_bottom
                              ~width:(Register_width.bits Hart_config.register_width)
                              instret
                        }
                      ]
                      @ upper_registers))
            }
          ;;
        end)
    in
    let unpriveleged_counters_and_timer_active = sel_top ~width:2 address ==:. 0b11 in
    let unpriveleged_counters_and_timers =
      Unpriveleged_counters_and_timers.create
        (Scope.sub_scope scope "unpriveleged$")
        { Unpriveleged_counters_and_timers.I.clock
        ; clear
        ; enable = enable &: unpriveleged_counters_and_timer_active
        ; is_write
        ; address =
            sel_bottom
              ~width:Unpriveleged_counters_and_timers.I.port_widths.address
              address
        ; write_value
        }
    in
    { O.valid = unpriveleged_counters_and_timers.valid
    ; value = unpriveleged_counters_and_timers.value
    }
  ;;

  let hierarchical ~clock_frequency (scope : Scope.t) (input : Signal.t I.t) =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ~scope ~name:"cs_registers" (create ~clock_frequency) input
  ;;
end
