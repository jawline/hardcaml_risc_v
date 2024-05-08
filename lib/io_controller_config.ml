type t =
  | No_io_controller
  | Uart_controller of Hardcaml_uart_controller.Config.t
