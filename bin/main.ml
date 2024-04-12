open! Core
open Hardcaml_risc_v
open Hardcaml_risc_v_hart
module Synth = Hardcaml_xilinx_reports

module Design =
  Cpu.Make
    (struct
      let address_width = Address_width.RV32
      let register_width = Register_width.B32
      let num_registers = 32
    end)
    (struct
      let num_bytes = 4096
    end)
    (struct
      let num_harts = 2
    end)

module Command = Synth.Command.With_interface (Design.I) (Design.O)

let command = Command.command_basic ~name:"Dual_hart" Design.create
let () = Command_unix.run command
