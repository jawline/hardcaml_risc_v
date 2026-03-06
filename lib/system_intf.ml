module type Config = sig
  val num_harts : int
  val include_io_controller : Io_controller_config.t
  val include_video_out : Video_config.t
  val memory_domain : Hardcaml_memory_controller.Clock_domain.t
end

module type Memory_config = sig
  val capacity_in_bytes : int
end
