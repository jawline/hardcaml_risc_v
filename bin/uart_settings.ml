open Hardcaml_uart

let default ~clock_frequency =
  { Config.baud_rate = 921600 (* TODO configurable: 115200 *)
  ; clock_frequency
  ; include_parity_bit = false
  ; stop_bits = 2
  }
;;
