open! Core
open Hardcaml_memory_controller

include Bram_memory_controller.Make (struct
    let capacity_in_bytes = 64 * 32 * 4
    let num_read_channels = 1
    let num_write_channels = 1
    let address_width = 32
    let data_bus_width = 32
    let cache_memory = None
  end)
