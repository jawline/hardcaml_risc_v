open! Core
open Hardcaml
open Hardcaml_uart_controller
module Report_synth = Hardcaml_xilinx_reports

let design_frequency = 50_000_000

module Config = struct
  let config =
    { Config.baud_rate = 9600
    ; clock_frequency = design_frequency
    ; include_parity_bit = false
    ; stop_bits = 2
    }
  ;;
end

module Rx = Uart_rx.Make (Config)
module Tx = Uart_tx.Make (Config)

module Machine = struct
  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; uart_rx : 'a
      }
    [@@deriving hardcaml ~rtlmangle:"$"]
  end

  module O = struct
    type 'a t = { uart_tx : 'a } [@@deriving hardcaml ~rtlmangle:"$"]
  end

  let create (scope : Scope.t) ({ I.clock; clear; uart_rx } : _ I.t) =
    let open Signal in
    let rx = Rx.hierarchical ~instance:"rx" scope { Rx.I.clock; clear; uart_rx } in
    let rdy = wire 1 in
    let buffer =
      Fifo.create
        ~showahead:true
        ~capacity:512
        ~clock
        ~clear
        ~wr:rx.data_out_valid
        ~d:rx.data_out
        ~rd:rdy
        ()
    in
    let tx =
      Tx.hierarchical
        ~instance:"tx"
        scope
        { Tx.I.clock; clear; data_in_valid = ~:(buffer.empty); data_in = buffer.q }
    in
    rdy <== tx.data_in_ready;
    { O.uart_tx = tx.uart_tx }
  ;;

  let hierarchical ~instance (scope : Scope.t) (input : Signal.t I.t) =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ~scope ~name:"uart_loopback" ~instance create input
  ;;
end

module Report_command = Report_synth.Command.With_interface (Machine.I) (Machine.O)

let report_command = Report_command.command_basic ~name:"Generate_top" Machine.create

module Rtl (I : Interface.S) (O : Interface.S) = struct
  let emit ~name ~directory (create : Scope.t -> Signal.t I.t -> Signal.t O.t) =
    let module M = Circuit.With_interface (I) (O) in
    printf "Emitting %s\n" name;
    Core_unix.mkdir_p directory;
    let scope = Scope.create ~flatten_design:false () in
    let circuit = M.create_exn ~name:"uart_loopback_top" (create scope) in
    Rtl.output
      ~database:(Scope.circuit_database scope)
      ~output_mode:(In_directory directory)
      Verilog
      circuit
  ;;
end

let rtl_command =
  let module M = Rtl (Machine.I) (Machine.O) in
  Command.basic
    ~summary:"generate RTL"
    (Command.Param.return (fun () ->
       M.emit
         ~name:"uart_loopback_top"
         ~directory:"./rtl/uart/"
         (Machine.hierarchical ~instance:"uart_loopback")))
;;

let all_commands =
  Command.group
    ~summary:"RTL tools"
    [ "report", report_command; "generate-rtl", rtl_command ]
;;

let () = Command_unix.run all_commands
