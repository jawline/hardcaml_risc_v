open! Core
open Hardcaml
open Hardcaml_risc_v
open Hardcaml_risc_v_hart
open Hardcaml_memory_controller
module Report_synth = Hardcaml_xilinx_reports

module Make_base (C : sig
    val include_video_out : bool
    val hart_frequency : int
    val capacity_in_bytes : int
  end) =
struct
  module Framebuffer_config = struct
    let output_width = 1024
    let output_height = 600
    let input_width = 64
    let input_height = 32
    let framebuffer_address = 0x8000
  end

  module Video_signal_generator_config = struct
    (* TODO: Add a clock requirement *)

    let h_active = 1024
    let v_active = 600
    let h_fp = 32
    let h_sync = 48
    let h_bp = 240
    let v_fp = 10
    let v_sync = 3
    let v_bp = 12
  end

  module General_config = struct
    let num_harts = 1

    let include_io_controller =
      Io_controller_config.Uart_controller
        (Uart_settings.default ~clock_frequency:C.hart_frequency)
    ;;

    let include_video_out =
      if C.include_video_out
      then
        Video_config.Video_out
          ( (module Framebuffer_config : Video_out_intf.Config)
          , (module Video_signal_generator_config : Video_signals.Config) )
      else No_video_out
    ;;
  end

  module Per_hart_config = struct
    let register_width = Register_width.B32
    let num_registers = 32
    let design_frequency = C.hart_frequency
  end

  module Memory_config = struct
    let capacity_in_bytes = C.capacity_in_bytes
  end

  let emit ~scope circuit =
    Rtl.print ~database:(Scope.circuit_database scope) Verilog circuit
  ;;
end

(** Construct a design with a generic AXI based memory interface and no internal BRAM based main memory. *)
module Make_with_axi_memory (C : sig
    val include_video_out : bool
    val hart_frequency : int
    val memory_tag_width : int
    val memory_width : int
    val memory_address_width : int
    val capacity_in_bytes : int
  end) =
struct
  open Make_base (C)

  module Axi_config = struct
    let id_width = C.memory_tag_width
    let data_width = C.memory_width
    let addr_width = C.memory_address_width
  end

  let () = Core.eprint_s [%message (C.memory_tag_width : int)]

  module Axi4 = Axi4.Make (Axi_config)

  module Design_with_bram =
    System.Make (Per_hart_config) (Memory_config) (General_config) (Axi_config) (Axi4)

  module Rtl = struct
    let emit () =
      let scope = Scope.create ~flatten_design:false () in
      let module M = Circuit.With_interface (Design_with_bram.I) (Design_with_bram.O) in
      let circuit =
        M.create_exn
          ~name:"top"
          (Design_with_bram.hierarchical ~build_mode:Synthesis scope)
      in
      emit ~scope circuit
    ;;
  end
end

(** Construct a design with BRAM based main memory. *)
module Make_with_bram (C : sig
    val include_video_out : bool
    val hart_frequency : int
    val capacity_in_bytes : int
  end) =
struct
  open Make_base (C)

  module Design_with_bram =
    System_with_bram.Make (Per_hart_config) (Memory_config) (General_config)

  module Rtl = struct
    let emit () =
      let scope = Scope.create ~flatten_design:false () in
      let module M = Circuit.With_interface (Design_with_bram.I) (Design_with_bram.O) in
      let circuit =
        M.create_exn
          ~name:"top"
          (Design_with_bram.hierarchical ~read_latency:2 ~build_mode:Synthesis scope)
      in
      emit ~scope circuit
    ;;
  end
end

let bram =
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
     and capacity_in_bytes =
       flag "memory-capacity-in-bytes" (required int) ~doc:"memory capacity in bytes"
     in
     fun () ->
       let module M =
         Make_with_bram (struct
           let include_video_out = include_video_out
           let hart_frequency = hart_frequency
           let capacity_in_bytes = capacity_in_bytes
         end)
       in
       M.Rtl.emit ())
;;

let axi =
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
     and capacity_in_bytes =
       flag "memory-capacity-in-bytes" (required int) ~doc:"memory capacity in bytes"
     and memory_tag_width =
       flag
         "memory-id-tag-width"
         (required int)
         ~doc:"width of AXI memory request tag ID in bits"
     and memory_address_width =
       flag "memory-addr-width" (required int) ~doc:"memory address width in bits"
     and memory_width =
       flag "memory-width" (required int) ~doc:"memory read/write width in bits"
     in
     fun () ->
       let module M =
         Make_with_axi_memory (struct
           let include_video_out = include_video_out
           let hart_frequency = hart_frequency
           let memory_tag_width = memory_tag_width
           let memory_width = memory_width
           let memory_address_width = memory_address_width
           let capacity_in_bytes = capacity_in_bytes
         end)
       in
       M.Rtl.emit ())
;;

let all_commands =
  Command.group
    ~summary:"RTL tools"
    [ "generate-rtl-with-bram", bram; "generate-rtl-with-axi-memory ", axi ]
;;

let () = Command_unix.run all_commands
