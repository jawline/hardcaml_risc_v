open Core
open Hardcaml

let program_ram sim bits =
  let ram = Cyclesim.lookup_mem_by_name sim "main_memory_bram" |> Option.value_exn in
  Array.iteri ~f:(fun i m -> Cyclesim.Memory.of_bits ~address:i ram m) bits
;;

let print_ram sim =
  let ram =
    Cyclesim.lookup_mem_by_name sim "main_memory_bram"
    |> Option.value_exn
    |> Cyclesim.Memory.read_all
  in
  Array.iter ~f:(fun v -> printf "%02x " (Bits.to_int_trunc v)) ram;
  printf "\n"
;;
