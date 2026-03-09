open! Core
open Hardcaml
open Signal

module Make (C : sig
    val width : int
  end) =
struct
  module I = struct
    type 'a t =
      { clock : 'a Clocking.t
      ; valid : 'a
      ; lhs : 'a [@bits C.width]
      ; rhs : 'a [@bits C.width]
      }
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t =
      { valid : 'a [@rtlprefix "output_"]
      ; value : 'a [@bits C.width]
      }
    [@@deriving hardcaml]
  end

  let steps = 3

  (* Cuts the LHS and RHS into the input for the given stage of the prefix multiplier. *)
  let which_input ~stage ~lhs ~rhs ~side =
    let arg =
      match side with
      | `Lhs -> lhs
      | `Rhs -> rhs
    in
    mux_init
      ~f:(fun stage ->
        match stage with
        | 0 -> List.nth_exn arg 0
        | 1 ->
          (match side with
           | `Lhs -> List.nth_exn arg 0
           | `Rhs -> List.nth_exn arg 1)
        | 2 ->
          (match side with
           | `Lhs -> List.nth_exn arg 1
           | `Rhs -> List.nth_exn arg 0)
        | _ -> assert false)
      stage
      steps
  ;;

  let create scope ({ I.clock; valid; lhs; rhs } : _ I.t) =
    let spec = Clocking.to_spec clock in
    let spec_no_clear = Clocking.to_spec_no_clear clock in
    let%hw lhs_hi, lhs_lo = split_in_half_msb lhs in
    let%hw rhs_hi, rhs_lo = split_in_half_msb rhs in
    let%hw working = wire 1 in
    let%hw finishing = wire 1 in
    let%hw stage =
      reg_fb
      ~clear:finishing
        ~enable:working
        ~width:(address_bits_for steps)
        ~f:(fun t -> mux2 valid (of_int_trunc ~width:(width t) 1) (t +:. 1))
        spec
    in
    working <-- (reg ~enable:(valid |: working) spec (valid |: ~:finishing) |: valid);
    finishing <-- (stage ==:. steps - 1);
    let%hw shift = reg ~clear:finishing ~enable:working spec vdd in
    let%hw lhs_input =
      which_input ~side:`Lhs ~lhs:[ lhs_lo; lhs_hi ] ~rhs:[ rhs_lo; rhs_hi ] ~stage
    in
    let%hw rhs_input =
      which_input ~side:`Rhs ~lhs:[ lhs_lo; lhs_hi ] ~rhs:[ rhs_lo; rhs_hi ] ~stage
    in
    let%hw acc =
      reg_fb
        ~width:(width lhs)
        ~enable:working
        ~f:(fun t ->
          let%hw multiplier_output = lhs_input *: rhs_input in
          let%hw shifted_result =
            mux2 shift (sll ~by:(width lhs_lo) multiplier_output) multiplier_output
          in
          let%hw shifted_result_trunc  = sel_bottom ~width:(width t) shifted_result in
          (* On the first cycle (the only unshifted cycle) we clear the
             accumulator, otherwise we add to it. *)
  let%hw accumulated_result =(t +: shifted_result_trunc) in
 
  let%hw next = 
          mux2 shift  accumulated_result shifted_result_trunc 
  in next)
        spec_no_clear
    in
    { O.valid = reg spec_no_clear finishing; value = acc }
  ;;

  let hierarchical (scope : Scope.t) (input : Signal.t I.t) =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ~scope ~name:"prefix_multiplier" create input
  ;;
end
