type t =
  | No_video_out
  | Video_out of ((module Video_out_intf.Config) * (module Video_signals.Config))
