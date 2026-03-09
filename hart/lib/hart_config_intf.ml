open Hardcaml_memory_controller

module type S = sig
  val register_width : Register_width.t
  val num_registers : int
  val clock_domain : Custom_clock_domain.t
  val register_fetch_output : bool
  val register_decode_output : bool

  module Extensions : sig
    val zmul : bool
  end
end
