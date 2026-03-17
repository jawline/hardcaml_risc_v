open! Core
open Hardcaml
open Signal

module type Config = sig
  val h_active : int
  val h_fp : int
  val h_sync : int
  val h_bp : int
  val v_active : int
  val v_fp : int
  val v_sync : int
  val v_bp : int
  val enable_every_n_cycles : int

  (* We can use this to drive the video signal from a faster multiple of the clock. *)
  val clock_domain : Hardcaml_memory_controller.Custom_clock_domain.t
end

module Video_signals = struct
  type 'a t =
    { video_clock : 'a
    ; video_active : 'a
    ; v_sync : 'a
    ; h_sync : 'a
    ; next_frame : 'a
    }
  [@@deriving hardcaml ~rtlmangle:"$"]
end

module Make (Config : Config) = struct
  let h_total_pixels = Config.h_active + Config.h_fp + Config.h_sync + Config.h_bp
  let v_total_lines = Config.v_active + Config.v_fp + Config.v_sync + Config.v_bp

  module I = struct
    type 'a t = { clock : 'a Clocking.t } [@@deriving hardcaml ~rtlmangle:"$"]
  end

  module O = Video_signals

  let create _scope (i : _ I.t) =
    (* This is pretty inefficient in comparisons and could be done with rollovers instead. *)
    let reg_spec = Clocking.to_spec i.clock in
    let enable_this_cycle =
      if Config.enable_every_n_cycles = 1
      then vdd
      else
        reg_fb
          ~width:(address_bits_for Config.enable_every_n_cycles)
          ~f:(mod_counter ~max:(Config.enable_every_n_cycles - 1))
          reg_spec
        ==:. 0
    in
    let h_cnt =
      reg_fb
        ~enable:enable_this_cycle
        ~width:(num_bits_to_represent (h_total_pixels - 1))
        ~f:(fun t -> mod_counter ~max:(h_total_pixels - 1) t)
        reg_spec
    in
    let is_end_of_line = h_cnt ==:. h_total_pixels - 1 in
    let last_beat_of_horizontal_front_porch = h_cnt ==:. Config.h_fp - 1 in
    let last_beat_of_horizontal_sync = h_cnt ==:. Config.h_fp + Config.h_sync - 1 in
    let last_beat_of_horizontal_back_porch =
      h_cnt ==:. Config.h_fp + Config.h_sync + Config.h_bp - 1
    in
    let v_cnt =
      reg_fb
        ~enable:enable_this_cycle
        ~width:(num_bits_to_represent (v_total_lines - 1))
        ~f:(fun t -> mux2 is_end_of_line (mod_counter ~max:(v_total_lines - 1) t) t)
        reg_spec
    in
    (* Pre-cache last-line to simplify the path a little. *)
    let last_line =
      reg ~enable:enable_this_cycle reg_spec (v_cnt ==:. v_total_lines - 1)
    in
    let next_frame = is_end_of_line &: last_line in
    let last_beat_of_vertical_front_porch = v_cnt ==:. Config.v_fp - 1 in
    let last_beat_of_vertical_sync = v_cnt ==:. Config.v_fp + Config.v_sync - 1 in
    let last_beat_of_vertical_back_porch =
      v_cnt ==:. Config.v_fp + Config.v_sync + Config.v_bp - 1
    in
    let is_last_line = v_cnt ==:. v_total_lines - 1 in
    (* Inside the real horizontal area of the signal rather than the margins *)
    let h_active =
      reg_fb
        ~enable:enable_this_cycle
        ~width:1
        ~f:(fun t ->
          mux2 last_beat_of_horizontal_back_porch vdd (mux2 is_end_of_line gnd t))
        reg_spec
    in
    (* Inside the real vertical area of the signal rather than the margins *)
    let v_active =
      reg_fb
        ~enable:enable_this_cycle
        ~width:1
        ~f:(fun t ->
          mux2
            (last_beat_of_horizontal_front_porch &: last_beat_of_vertical_back_porch)
            vdd
            (mux2 (is_last_line &: last_beat_of_horizontal_front_porch) gnd t))
        reg_spec
    in
    let h_sync =
      (* The horizontal line signal is
         [data] [ front porch ] [ sync ] [ back porch ] *)
      reg_fb
        ~enable:enable_this_cycle
        ~width:1
        ~f:(fun t ->
          mux2
            last_beat_of_horizontal_front_porch
            vdd
            (mux2 last_beat_of_horizontal_sync gnd t))
        reg_spec
    in
    let v_sync =
      reg_fb
        ~enable:enable_this_cycle
        ~width:1
        ~f:(fun t ->
          let start =
            last_beat_of_horizontal_front_porch &: last_beat_of_vertical_front_porch
          in
          let end_ = last_beat_of_vertical_sync &: last_beat_of_horizontal_front_porch in
          mux2 start vdd (mux2 end_ gnd t))
        reg_spec
    in
    (* We are draining data now and not margin *)
    let video_active = v_active &: h_active in
    { O.video_clock = enable_this_cycle; h_sync; v_sync; video_active; next_frame }
  ;;

  let hierarchical (scope : Scope.t) =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ~scope ~name:"video_signals" create
  ;;
end
