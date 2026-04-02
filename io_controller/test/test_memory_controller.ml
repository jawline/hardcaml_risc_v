open Hardcaml_memory_controller

module Instruction_config : Shared_access_ports_intf.Config = struct
  let num_read_channels = 0
  let num_write_channels = 0
  let cache_memory = None
end

module Data_config : Shared_access_ports_intf.Config = struct
  let num_read_channels = 1
  let num_write_channels = 1
  let cache_memory = None
end

include Bram_memory_controller.Make (struct
    let capacity_in_bytes = 65536
    let address_width = 32
    let data_bus_width = 32

    module Instruction_config = Instruction_config
    module Data_config = Data_config
  end)
