open! Core
open Hardcaml
open Hardcaml_risc_v
open Hardcaml_risc_v_hart
module Report_synth = Hardcaml_xilinx_reports

let design_frequency = 180_000

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
      let num_harts = 1

      let include_io_controller =
        Io_controller_config.Uart_controller
          { baud_rate = 9600
          ; clock_frequency = design_frequency
          ; include_parity_bit = true
          ; stop_bits = 1
          }
      ;;
    end)

module Report_command = Report_synth.Command.With_interface (Design.I) (Design.O)

let report_command = Report_command.command_basic ~name:"Generate_top" Design.create

module Rtl = struct
  let emit
    (type i o)
    ~name
    ~directory
    (module I : Interface.S_Of_signal with type Of_signal.t = i)
    (module O : Interface.S_Of_signal with type Of_signal.t = o)
    create
    =
    printf "Emitting %s\n" name;
    Core_unix.mkdir_p directory;
    let scope = Scope.create ~flatten_design:false () in
    let circuit =
      Circuit.create_with_interface ~name (module I) (module O) (create scope)
    in
    Rtl.output
      ~database:(Scope.circuit_database scope)
      ~output_mode:(In_directory directory)
      Verilog
      circuit
  ;;

  let command =
    Command.basic ~summary:"generate RTL"
      (Command.Param.return (fun () -> 
    emit
      ~name:"cpu_top"
      ~directory:"./rtl/cpu/"
      (module Design.I)
      (module Design.O)
      (Design.hierarchical ~instance:"cpu")))
      ;;
end

let all_commands = Command.group ~summary:"RTL tools" [ "report", report_command ; "generate-rtl" , Rtl.command ]
let () = Command_unix.run all_commands
