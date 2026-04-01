open Hardcaml_risc_v_hart

let register_width = Register_width.B32
let num_registers = 32
let clock_domain = Hardcaml_memory_controller.Custom_clock_domain.create 50_000_000
let register_fetch_output = true
let register_decode_output = true
let register_execute_output = true

module Extensions = struct
  let zmul = false
  let zba = true
end
