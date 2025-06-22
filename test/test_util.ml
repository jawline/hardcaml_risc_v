open Core
open Hardcaml

let print_ram sim =
  let ram =
    Cyclesim.lookup_mem_by_name sim "main_memory_bram"
    |> Option.value_exn
    |> Cyclesim.Memory.read_all
    |> Array.to_list
    |> Bits.concat_lsb
    |> Bits.split_lsb ~part_width:8
    |> List.map ~f:Bits.to_char
    |> String.of_char_list
  in
  print_s [%message "" ~_:(ram : String.Hexdump.t)]
;;

let program_ram sim memory_parts =
  (* Split bits from whatever size encoding it is (probably instruction width) into bytes. *)
  let bits =
    Array.to_list memory_parts |> Bits.concat_lsb |> Bits.split_lsb ~part_width:8
  in
  let ram = Cyclesim.lookup_mem_by_name sim "main_memory_bram" |> Option.value_exn in
  List.iteri ~f:(fun i m -> Cyclesim.Memory.of_bits ~address:i ram m) bits
;;
