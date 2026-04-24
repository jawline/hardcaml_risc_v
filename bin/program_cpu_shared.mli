open! Core

val read_byte : in_channel:In_channel.t -> unit -> int
val read_packet : In_channel.t -> char * int * string
val do_write : ch:Out_channel.t -> int list -> unit

val open_with_stty_settings
  :  baud_rate:int
  -> stop_bits:int
  -> parity_bit:bool
  -> device_filename:string
  -> Out_channel.t * In_channel.t

val send_chunk : writer:Out_channel.t -> address:int -> chunk:string -> unit
val print_any_incoming_packets : reader:In_channel.t -> unit

val program_cpu
  :  skip_program:bool
  -> device_filename:string
  -> program_filename:string
  -> Out_channel.t * In_channel.t
