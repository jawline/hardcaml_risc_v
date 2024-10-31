open! Core
open Hardcaml
open! Bits

let write_packet_to_memory ~packet sim =
  let ram = Cyclesim.lookup_mem_by_name sim "main_memory_bram" |> Option.value_exn in
  let packet = String.to_array packet in
  for ram_address = 0 to Array.length packet / 4 do
    let start = ram_address * 4 in
    let sel idx =
      if start + idx < Array.length packet then packet.(start + idx) else '\x00'
    in
    let new_bits = List.init ~f:sel 4 |> List.map ~f:Bits.of_char |> Bits.concat_lsb in
    Cyclesim.Memory.of_bits ~address:ram_address ram new_bits
  done
;;

let print_ram sim =
  let as_str =
    Cyclesim.lookup_mem_by_name sim "main_memory_bram"
    |> Option.value_exn
    |> Cyclesim.Memory.read_all
    |> Array.to_list
    |> List.map ~f:(fun t -> Bits.split_lsb ~part_width:8 t |> List.map ~f:Bits.to_char)
    |> List.concat
    |> String.of_char_list
  in
  print_s [%message "" ~_:(as_str : String.Hexdump.t)]
;;
