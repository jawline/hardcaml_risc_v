type t =
  | No_io_controller
  | Uart_controller of Hardcaml_uart.Config.t
