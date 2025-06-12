open! Core
open Hardcaml
open Hardcaml_risc_v
open Hardcaml_risc_v_hart
module Report_synth = Hardcaml_xilinx_reports

module Make (C : sig
    val include_video_out : bool
    val hart_frequency : int
  end) =
struct
  module Design =
    System.Make
      (struct
        let register_width = Register_width.B32
        let num_registers = 32
        let design_frequency = C.hart_frequency
      end)
      (struct
        let num_bytes = 65536
      end)
      (struct
        let num_harts = 1

        let include_io_controller =
          Io_controller_config.Uart_controller
            (Uart_settings.default ~clock_frequency:C.hart_frequency)
        ;;

        let include_video_out =
          if C.include_video_out
          then
            Video_config.Video_out
              ( (module struct
                  let output_width = 1024
                  let output_height = 600
                  let input_width = 64
                  let input_height = 32
                  let framebuffer_address = 0x8000
                end : Video_out_intf.Config)
              , (module struct
                  (* TODO: Add a clock requirement *)

                  let h_active = 1024
                  let v_active = 600
                  let h_fp = 32
                  let h_sync = 48
                  let h_bp = 240
                  let v_fp = 10
                  let v_sync = 3
                  let v_bp = 12
                end : Video_signals.Config) )
          else No_video_out
        ;;
      end)

  module Report_command = Report_synth.Command.With_interface (Design.I) (Design.O)

  let report_command =
    Report_command.command_basic ~name:"Generate_top" Design.hierarchical
  ;;

  module Rtl = struct
    let emit () =
      let module M = Circuit.With_interface (Design.I) (Design.O) in
      let scope = Scope.create ~flatten_design:false () in
      let circuit = M.create_exn ~name:"top" (Design.hierarchical scope) in
      Rtl.print ~database:(Scope.circuit_database scope) Verilog circuit
    ;;
  end
end

let rtl_command =
  Command.basic
    ~summary:"generate RTL"
    (let open Command.Let_syntax in
     let open Command.Param in
     let%map include_video_out =
       flag
         "include-video-out"
         (required bool)
         ~doc:"include logic for generating a video signal"
     and hart_frequency =
       flag "hart-frequency" (required int) ~doc:"clock frequency in hz for the hart"
     in
     fun () ->
       let module M =
         Make (struct
           let include_video_out = include_video_out
           let hart_frequency = hart_frequency
         end)
       in
       M.Rtl.emit ())
;;

module Without_video = Make (struct
    let include_video_out = false
    let hart_frequency = 100_000_000
  end)

module With_video = Make (struct
    let include_video_out = true
    let hart_frequency = 100_000_000
  end)

let all_commands =
  Command.group
    ~summary:"RTL tools"
    [ "report", With_video.report_command
    ; "report-novideo", Without_video.report_command
    ; "generate-rtl", rtl_command
    ]
;;

let () = Command_unix.run all_commands
