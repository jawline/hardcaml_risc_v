module type Config = sig
  val num_harts : int
  val include_io_controller : Io_controller_config.t
  val include_video_out : Video_config.t
end

module type Memory_config = sig
  val capacity_in_bytes : int
end
