module type S = sig
  val register_width : Register_width.t
  val num_registers : int
  val design_frequency : int

  module Extensions : sig
    val zmul : bool
  end
end
