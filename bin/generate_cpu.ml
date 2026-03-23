open! Core
open Hardcaml
open Hardcaml_risc_v
open Hardcaml_risc_v_hart
open Hardcaml_memory_controller
module Report_synth = Hardcaml_xilinx_reports

module Make_base (C : sig
    val memory_clock : Custom_clock_domain.t
    val hart_clock : Custom_clock_domain.t
    val video_clock : Custom_clock_domain.t
    val capacity_in_bytes : int
    val framebuffer_address_in_memory : int
    val include_video_out : bool
    val include_cache_with_n_lines : int option
  end) =
struct
  let round_nearest_div x y = Int.round x ~to_multiple_of:y ~dir:`Nearest / y

  (*
  module Video_signal_generator_config_640_480_60hz = struct
    (* 25~Mhz. *)
    let h_active = 640
    let v_active = 480
    let h_fp = 16
    let h_sync = 96
    let h_bp = 48
    let v_fp = 10
    let v_sync = 2
    let v_bp = 33
    let clock_domain = C.video_clock
    let enable_every_n_cycles = round_nearest_div C.video_clock.frequency 25_000_000
    let _ = enable_every_n_cycles (* TODO: MLI so this doesn't get marked as unused *)
  end

  module Video_signal_generator_config_640_480_30hz = struct
    include Video_signal_generator_config_640_480_60hz

    let enable_every_n_cycles = round_nearest_div C.video_clock.frequency 12_500_000
  end *)

  module Video_signal_generator_config_800_600_60hz = struct
    (* 40.000 Mhz. *)
    let h_active = 800
    let v_active = 600
    let h_fp = 40
    let h_sync = 128
    let h_bp = 88
    let v_fp = 1
    let v_sync = 4
    let v_bp = 23
    let clock_domain = C.video_clock
    let enable_every_n_cycles = round_nearest_div C.video_clock.frequency 40_000_000
    let _ = enable_every_n_cycles
  end

  module Used_video = Video_signal_generator_config_800_600_60hz

  module Framebuffer_config = struct
    let output_width = Used_video.h_active
    let output_height = Used_video.v_active
    let input_width = 320
    let input_height = 200
    let framebuffer_address = C.framebuffer_address_in_memory
    let input_pixel_mode = Hardcaml_framebuffer_expander.Pixel_mode.RGB_8bit_32bit_aligned
  end

  (* 
  module Video_signal_generator_config_1024_600 = struct

          (* 50MHz *)
    let h_active = 1024
    let v_active = 600
    let h_fp = 32
    let h_sync = 48
    let h_bp = 240
    let v_fp = 10
    let v_sync = 3
    let v_bp = 12
    let clock_domain = C.video_clock
    let enable_every_n_cycles = 
    C.video_clock.frequency / 25_000_000
  end *)

  module General_config = struct
    let num_harts = 1

    let include_io_controller =
      Io_controller_config.Uart_controller
        (Uart_settings.default ~clock_frequency:C.hart_clock.frequency)
    ;;

    let include_video_out =
      if C.include_video_out
      then
        Video_config.Video_out
          ( (module Framebuffer_config : Video_out_intf.Config)
          , (module Used_video : Video_signals.Config) )
      else No_video_out
    ;;

    let memory_domain = C.memory_clock
    let dma_domain = C.hart_clock

    let include_cache =
      match C.include_cache_with_n_lines with
      | Some num_lines ->
        Some
          (module struct

            let line_width = 16
            let num_cache_lines = num_lines
            let register_responses = true (* TODO: Make this configurable *)
            let register_axi_requests = true
          end : System_intf.Cache_config)
      | None -> None
    ;;
  end

  module Per_hart_config = struct
    let register_width = Register_width.B32
    let num_registers = 32

    module Extensions = struct
      let zmul = true
    end

    let clock_domain = C.hart_clock
    let register_fetch_output = true
    let register_decode_output = true
    let register_execute_output = true
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
    val memory_tag_width : int
    val memory_width : int
    val memory_address_width : int
    val capacity_in_bytes : int
    val burst_length_bits : int
    val memory_clock : Custom_clock_domain.t
    val hart_clock : Custom_clock_domain.t
    val video_clock : Custom_clock_domain.t
    val framebuffer_address_in_memory : int
    val include_video_out : bool
    val include_cache_with_n_lines : int option
  end) =
struct
  open Make_base (struct
      include C
    end)

  module Axi_config = struct
    let id_bits = C.memory_tag_width
    let data_bits = C.memory_width
    let addr_bits = C.memory_address_width
    let burst_length_bits = C.burst_length_bits
  end

  module Axi4 = Axi4.Make (Axi_config)

  module Design_with_bram =
    System.Make (Per_hart_config) (Memory_config) (General_config) (Axi4)

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
    val capacity_in_bytes : int
    val hart_clock : Custom_clock_domain.t
    val video_clock : Custom_clock_domain.t
    val framebuffer_address_in_memory : int
    val include_video_out : bool
  end) =
struct
  open Make_base (struct
      include C

      let memory_clock = hart_clock
      let include_cache_with_n_lines = None
    end)

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
         ~doc:"include logic for generating a video signal."
     and hart_frequency =
       flag "hart-frequency" (required int) ~doc:"clock frequency in hz for the hart"
     and video_frequency =
       flag
         "video-frequency"
         (required int)
         ~doc:"video frequency in hz for the video controller (ignored if unused)"
     and capacity_in_bytes =
       flag "memory-capacity-in-bytes" (required int) ~doc:"memory capacity in bytes"
     and framebuffer_address_in_memory =
       flag
         "framebuffer-address-in-memory"
         (required int)
         ~doc:"location of the framebuffer in memory"
     in
     fun () ->
       let module M =
         Make_with_bram (struct
           let hart_frequency = hart_frequency
           let capacity_in_bytes = capacity_in_bytes
           let video_clock = Custom_clock_domain.create video_frequency
           let hart_clock = Custom_clock_domain.create hart_frequency
           let framebuffer_address_in_memory = framebuffer_address_in_memory
           let include_video_out = include_video_out
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
         ~doc:
           "include logic for generating a video signal. integer specifies the video \
            clock ratio."
     and video_frequency =
       flag
         "video-frequency"
         (required int)
         ~doc:"video frequency in hz for the video controller (ignored if unused)"
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
     and burst_length_bits =
       flag
         "burst-length-bits"
         (required int)
         ~doc:"width of the burst length (AWLEN, ARLEN) in bits"
     and memory_frequency =
       flag
         "memory-frequency"
         (required int)
         ~doc:"clock frequency in hz for the memory controller"
     and framebuffer_address_in_memory =
       flag
         "framebuffer-address-in-memory"
         (required int)
         ~doc:"location of the framebuffer in memory"
     and include_cache =
       flag "include-cache" (optional int) ~doc:"devote board BRAM to a memory cache"
     in
     let memory_clock = Custom_clock_domain.create memory_frequency in
     let hart_clock =
       if hart_frequency = memory_frequency
       then memory_clock
       else Custom_clock_domain.create hart_frequency
     in
     let video_clock =
       if video_frequency = memory_frequency
       then memory_clock
       else Custom_clock_domain.create video_frequency
     in
     fun () ->
       let module M =
         Make_with_axi_memory (struct
           let memory_tag_width = memory_tag_width
           let memory_width = memory_width
           let memory_address_width = memory_address_width
           let capacity_in_bytes = capacity_in_bytes
           let video_clock = video_clock
           let hart_clock = hart_clock
           let memory_clock = memory_clock
           let burst_length_bits = burst_length_bits
           let framebuffer_address_in_memory = framebuffer_address_in_memory
           let include_cache_with_n_lines = include_cache
           let include_video_out = include_video_out
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
