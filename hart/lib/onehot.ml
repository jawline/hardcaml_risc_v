open! Core
open Hardcaml
open Signal

module Make (C : Onehot_intf.Config) = struct
  let max_rank = List.length C.all - 1
  let width = num_bits_to_represent (1 lsl max_rank)

  type 'a t = { packed : 'a [@bits width] } [@@deriving hardcaml]

  let valid (t : _ t) code = t.packed.:(C.Variants.to_rank code)
  let construct_onehot ~f = { packed = C.all |> List.map ~f |> concat_lsb }
  let construct_onehot_bits ~f = { packed = C.all |> List.map ~f |> Bits.concat_lsb }

  let switch ~f t =
    onehot_select
      ((List.map ~f:(fun code -> { With_valid.valid = valid t code; value = f code }))
         C.all)
  ;;
end
