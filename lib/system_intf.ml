module type Cache_config = sig
  val line_width : int
  val num_cache_lines : int
  val register_responses : bool
  val register_axi_requests : bool
end

module type Config = sig
  val num_harts : int
  val include_io_controller : Io_controller_config.t
  val include_video_out : Video_config.t
  val dma_domain : Hardcaml_memory_controller.Custom_clock_domain.t
  val memory_domain : Hardcaml_memory_controller.Custom_clock_domain.t
  val include_instruction_cache : (module Cache_config) option
  val include_data_cache : (module Cache_config) option
end

module type Memory_config = sig
  val capacity_in_bytes : int
end
