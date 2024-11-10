type t =
  | No_video_out
  | Video_out of
      { output_width : int
      ; output_height : int
      ; framebuffer_width : int
      ; framebuffer_height : int
      }
