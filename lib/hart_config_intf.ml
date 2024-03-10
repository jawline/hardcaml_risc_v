module type S = sig
  val address_width : Address_width.t
  val register_width : Register_width.t
  val num_registers : int
end
