open! Core
open Hardcaml
open Signal

module Make (Hart_config : Hart_config_intf.S) = struct
  module I = struct
    type 'a t =
      { clock : 'a Clocking.t
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
        ~initialize_registers_to
        ~clock_frequency
        scope
        { I.clock; instret; enable; is_write; address; write_value }
    =
    let spec = Clocking.to_spec clock in
    let cycle =
      reg_fb
        ~clear_to:(Signal.of_bits initialize_registers_to)
        ~initialize_to:initialize_registers_to
        spec
        ~width:64
        ~f:(fun t -> t +:. 1)
    in
    let ns_per_second = 1_000_000_000 in
    let ns_per_cycle = ns_per_second / clock_frequency in
    let clock_adjustment =
      let open Float in
      let real_ns_per_cycle = of_int ns_per_second / of_int clock_frequency in
      let missing_portion_of_clock = real_ns_per_cycle - of_int ns_per_cycle in
      if missing_portion_of_clock <> 0.
      then (
        let adjust_ctr = 1. / missing_portion_of_clock in
        if not (Float.is_integer adjust_ctr)
        then
          eprint_s
            [%message
              "WARNING: The clock incrementer cannot correctly adjust time for the clock \
               of this device. This will lead to a particularly unreliable ns clock on \
               the device."
                (clock_frequency : int)
                (ns_per_second : int)
                (real_ns_per_cycle : float)
                (adjust_ctr : float)];
        Some (to_int adjust_ctr))
      else None
    in
    let clock_adjust_ctr =
      match clock_adjustment with
      | Some clock_adjustment ->
        Some
          (reg_fb
             ~width:(address_bits_for clock_adjustment)
             ~f:(mod_counter ~max:(clock_adjustment - 1))
             spec)
      | None -> None
    in
    let time =
      (* Increment on cycle by ns per cycle *)
      reg_fb
        ~clear_to:(Signal.of_bits initialize_registers_to)
        ~initialize_to:initialize_registers_to
        spec
        ~width:64
        ~f:(fun t ->
          let result_base = t +:. ns_per_cycle in
          match clock_adjust_ctr with
          | Some clock_adjust_ctr ->
            result_base +: mux2 (clock_adjust_ctr ==:. 0) (one (width t)) (zero (width t))
          | None -> result_base)
    in
    let instret =
      reg_fb
        ~clear_to:(Signal.of_bits initialize_registers_to)
        ~initialize_to:initialize_registers_to
        spec
        ~enable:instret
        ~width:64
        ~f:(fun t -> t +:. 1)
    in
    let%hw top_time =
      sel_top ~width:(Register_width.bits Hart_config.register_width) time
    in
    let module Unpriveleged_counters_and_timers =
      (* TODO: In hindsight constructing these modules in line is a bad idea because you can't use hierarchical and you can accidentally use scope like address that you shouldn't. Move it out to it's own module and just instantiate the registers there. *)
        Cs_register_bank.Make
          (Hart_config)
          (struct
            let address_bits = 10

            (* These registers are only defined in 32b processor variants, as
             the register reads are not sufficient for the whole register. *)
            let upper_registers ~valid:_ ~address ~also_write:_ ~value:_ =
              match Hart_config.register_width with
              | B32 ->
                [ { With_valid.valid = address ==:. 0x80
                  ; value =
                      sel_top
                        ~width:(Register_width.bits Hart_config.register_width)
                        cycle
                  }
                ; { valid = address ==:. 0x81; value = top_time }
                ; { valid = address ==:. 0x82
                  ; value =
                      sel_top
                        ~width:(Register_width.bits Hart_config.register_width)
                        instret
                  }
                ]
            ;;

            let register_io ~valid ~address ~also_write ~value =
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
                        @ upper_registers ~valid ~address ~also_write ~value))
              }
            ;;
          end)
    in
    let unpriveleged_counters_and_timers =
      let%hw unpriveleged_counters_and_timer_active =
        sel_top ~width:2 address ==:. 0b11
      in
      let%hw sub_address =
        sel_bottom ~width:Unpriveleged_counters_and_timers.I.port_widths.address address
      in
      Unpriveleged_counters_and_timers.create
        (Scope.sub_scope scope "unpriveleged$")
        { Unpriveleged_counters_and_timers.I.clock
        ; enable = enable &: unpriveleged_counters_and_timer_active
        ; is_write
        ; address = sub_address
        ; write_value
        }
    in
    { O.valid = unpriveleged_counters_and_timers.valid
    ; value = unpriveleged_counters_and_timers.value
    }
  ;;

  let hierarchical
        ~initialize_registers_to
        ~clock_frequency
        (scope : Scope.t)
        (input : Signal.t I.t)
    =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical
      ~scope
      ~name:"cs_registers"
      (create ~initialize_registers_to ~clock_frequency)
      input
  ;;
end
