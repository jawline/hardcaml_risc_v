open Hardcaml_axi

include Stream.Make (struct
    let data_bits = 8
    let user_bits = 1
  end)
