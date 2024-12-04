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
end

module Video_signals = struct
  type 'a t =
    { video_active : 'a
    ; v_sync : 'a
    ; h_sync : 'a
    }
  [@@deriving hardcaml ~rtlmangle:"$"]
end

module Make (Config : Config) = struct
  let h_total_pixels = Config.h_active + Config.h_fp + Config.h_sync + Config.h_bp
  let v_total_lines = Config.v_active + Config.v_fp + Config.v_sync + Config.v_bp

  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      }
    [@@deriving hardcaml ~rtlmangle:"$"]
  end

  module O = Video_signals

  let create _scope (i : _ I.t) =
    (* This is pretty inefficient in comparisons and could be done with rollovers instead. *)
    let reg_spec = Reg_spec.create ~clock:i.clock ~clear:i.clear () in
    let h_cnt =
      reg_fb
        ~width:(num_bits_to_represent (h_total_pixels - 1))
        ~f:(fun t -> mod_counter ~max:(h_total_pixels - 1) t)
        reg_spec
    in
    let is_end_of_line = h_cnt ==:. h_total_pixels - 1 in
    let v_cnt =
      reg_fb
        ~width:(num_bits_to_represent (v_total_lines - 1))
        ~f:(fun t -> mux2 is_end_of_line (mod_counter ~max:(v_total_lines - 1) t) t)
        reg_spec
    in
    let is_last_line = v_cnt ==:. v_total_lines - 1 in
    (* Inside the real horizontal area of the signal rather than the margins *)
    let h_active =
      reg_fb
        ~width:1
        ~f:(fun t ->
          mux2
            (h_cnt ==:. Config.h_fp + Config.h_sync + Config.h_bp - 1)
            vdd
            (mux2 is_end_of_line gnd t))
        reg_spec
    in
    (* Inside the real vertical area of the signal rather than the margins *)
    let v_active =
      reg_fb
        ~width:1
        ~f:(fun t ->
          mux2
            (h_cnt
             ==:. Config.h_fp - 1
             &: (v_cnt ==:. Config.v_fp + Config.v_sync + Config.v_bp - 1))
            vdd
            (mux2 (is_last_line &: (h_cnt ==:. Config.h_fp - 1)) gnd t))
        reg_spec
    in
    let h_sync =
      reg_fb
        ~width:1
        ~f:(fun t ->
          let start = h_cnt ==:. Config.h_fp - 1 in
          let end_ = h_cnt ==:. h_total_pixels - 1 in
          mux2 start vdd (mux2 end_ gnd t))
        reg_spec
    in
    let v_sync =
      reg_fb
        ~width:1
        ~f:(fun t ->
          let start = v_cnt ==:. Config.v_fp - 1 &: (h_cnt ==:. Config.h_fp - 1) in
          let end_ =
            v_cnt ==:. Config.v_fp + Config.v_sync - 1 &: (h_cnt ==:. Config.h_fp - 1)
          in
          mux2 start vdd (mux2 end_ gnd t))
        reg_spec
    in
    (* We are draining data now and not margin *)
    let video_active = v_active &: h_active in
    { O.h_sync; v_sync; video_active }
  ;;

  let hierarchical (scope : Scope.t) =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ~scope ~name:"video_signals" create
  ;;
end
