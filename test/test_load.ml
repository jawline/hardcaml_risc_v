open! Core
open Hardcaml
open Risc_v_hardcaml
open! Bits

module Hart_config = struct
  let address_width = Address_width.RV32
  let register_width = Register_width.B32
  let num_registers = 32
end

module Memory_controller = Memory_controller.Make (struct
    let num_bytes = 4096
    let num_channels = 1
    let address_width = 32
    let data_bus_width = 32
  end)

module Load = Load.Make (Hart_config) (Memory_controller)
