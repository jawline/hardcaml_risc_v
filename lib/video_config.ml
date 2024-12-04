type t =
  | No_video_out
  | Video_out of ((module Video_out.Config) * (module Video_signals.Config))
