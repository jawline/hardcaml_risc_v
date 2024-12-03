open! Core
open Hardcaml
open Hardcaml_waveterm
open Hardcaml_uart_controller
open Hardcaml_io_controller
open! Bits

let debug = false

let test ~name ~clock_frequency ~baud_rate ~include_parity_bit ~stop_bits ~packet =
  let all_inputs =
    (* We add the header and then the packet length before the packet *)
    let packet = String.to_list packet in
    let packet_len_parts =
      Bits.of_int ~width:16 (List.length packet)
      |> split_msb ~part_width:8
      |> List.map ~f:Bits.to_int
    in
    [ Char.to_int 'Q' ] @ packet_len_parts @ List.map ~f:Char.to_int packet
  in
  let module Config = struct
    (* This should trigger a switch every other cycle. *)
    let config =
      { Hardcaml_uart_controller.Config.clock_frequency
      ; baud_rate
      ; include_parity_bit
      ; stop_bits
      }
    ;;
  end
  in
  let module Packet =
    Packet.Make (struct
      let data_bus_width = 8
    end)
  in
  let module Uart_tx = Uart_tx.Make (Config) in
  let module Uart_rx = Uart_rx.Make (Config) in
  let module Serial_to_packet =
    Serial_to_packet.Make
      (struct
        let header = 'Q'
        let serial_input_width = 8
        let max_packet_length_in_data_widths = 16
      end)
      (Packet)
  in
  let module Router =
    Router.Make
      (struct
        let num_tags = 2
      end)
      (Packet)
  in
  let module Pulse = Pulse.Make (Packet) in
  let module Machine = struct
    open Signal

    module I = struct
      type 'a t =
        { clock : 'a
        ; clear : 'a
        ; data_in_valid : 'a
        ; data_in : 'a [@bits 8]
        }
      [@@deriving hardcaml]
    end

    module O = struct
      type 'a t =
        { pulse_1 : 'a
        ; pulse_2 : 'a
        }
      [@@deriving hardcaml]
    end

    let create (scope : Scope.t) { I.clock; clear; data_in_valid; data_in } =
      let { Uart_tx.O.uart_tx; _ } =
        Uart_tx.hierarchical
          ~instance:"tx"
          scope
          { Uart_tx.I.clock; clear; data_in_valid; data_in }
      in
      let { Uart_rx.O.data_out_valid; data_out; parity_error = _ } =
        Uart_rx.hierarchical
          ~instance:"rx"
          scope
          { Uart_rx.I.clock; clear; uart_rx = uart_tx }
      in
      let { Serial_to_packet.O.out } =
        Serial_to_packet.hierarchical
          ~instance:"serial_to_packet"
          scope
          { Serial_to_packet.I.clock
          ; clear
          ; in_valid = data_out_valid
          ; in_data = data_out
          ; out = { ready = vdd }
          }
      in
      let pulse_1_ready = wire 1 in
      let pulse_2_ready = wire 1 in
      let router =
        Router.hierarchical
          ~instance:"router"
          scope
          { Router.I.clock
          ; clear
          ; in_ = out
          ; outs = [ { ready = pulse_1_ready }; { ready = pulse_2_ready } ]
          }
      in
      let pulse_1 =
        Pulse.hierarchical
          ~instance:"pulse_1"
          scope
          { Pulse.I.clock; clear; in_ = List.nth_exn router.outs 0 }
      in
      let pulse_2 =
        Pulse.hierarchical
          ~instance:"pulse_2"
          scope
          { Pulse.I.clock; clear; in_ = List.nth_exn router.outs 1 }
      in
      pulse_1_ready <== pulse_1.in_.ready;
      pulse_2_ready <== pulse_2.in_.ready;
      { O.pulse_1 = pulse_1.signal; pulse_2 = pulse_2.signal }
    ;;
  end
  in
  let create_sim () =
    let module Sim = Cyclesim.With_interface (Machine.I) (Machine.O) in
    Sim.create
      ~config:Cyclesim.Config.trace_all
      (Machine.create
         (Scope.create ~auto_label_hierarchical_ports:true ~flatten_design:true ()))
  in
  let sim = create_sim () in
  let waveform, sim = Waveform.create sim in
  let inputs : _ Machine.I.t = Cyclesim.inputs sim in
  let outputs : _ Machine.O.t = Cyclesim.outputs sim in
  (* The fifo needs a clear cycle to initialize *)
  inputs.clear := vdd;
  Cyclesim.cycle sim;
  Cyclesim.cycle sim;
  Cyclesim.cycle sim;
  inputs.clear := gnd;
  let rec loop_for n =
    if n = 0
    then ()
    else (
      Cyclesim.cycle sim;
      if Bits.to_bool !(outputs.pulse_1) then print_s [%message "Pulse 1 pulsed"];
      if Bits.to_bool !(outputs.pulse_2) then print_s [%message "Pulse 2 pulsed"];
      loop_for (n - 1))
  in
  List.iter
    ~f:(fun input ->
      inputs.data_in_valid := vdd;
      inputs.data_in := of_int ~width:8 input;
      Cyclesim.cycle sim;
      inputs.data_in_valid := of_int ~width:1 0;
      loop_for 44)
    all_inputs;
  loop_for 500;
  if debug
  then Waveform.expect ~serialize_to:name ~display_width:150 ~display_height:100 waveform
;;

let%expect_test "test" =
  test
    ~name:"/tmp/test_router"
    ~clock_frequency:200
    ~baud_rate:50
    ~include_parity_bit:false
    ~stop_bits:1
    ~packet:"\x00H";
  [%expect {|
      "Pulse 1 pulsed" |}];
  test
    ~name:"/tmp/test_router"
    ~clock_frequency:200
    ~baud_rate:50
    ~include_parity_bit:false
    ~stop_bits:1
    ~packet:"\x01H";
  [%expect {|
      "Pulse 2 pulsed" |}]
;;
