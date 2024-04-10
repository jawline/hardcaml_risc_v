open! Core
open Hardcaml
open Hardcaml_waveterm
open Hardcaml_uart_controller
open Hardcaml_io_controller
open! Bits

let debug = false

let test ~name ~clock_frequency ~baud_rate ~include_parity_bit ~stop_bits ~packet =
  let all_inputs =
    (* We add the magic and then the packet length before the packet *)
    let packet = String.to_list packet in
    let packet_size = List.length packet in
    let packet_len_msb = packet_size land 0xFF00 in
    let packet_len_lsb = packet_size land 0x00FF in
    [ Char.to_int 'Q'; packet_len_msb; packet_len_lsb ] @ List.map ~f:Char.to_int packet
  in
  let module Config = struct
    (* This should trigger a switch every other cycle. *)
    let clock_frequency = clock_frequency
    let baud_rate = baud_rate
    let include_parity_bit = include_parity_bit
    let stop_bits = stop_bits
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
        let magic = 'Q'
        let serial_input_width = 8
        let max_packet_length_in_data_widths = 16
      end)
      (Packet)
  in
  let module Machine = struct
    open Signal

    module I = struct
      type 'a t =
        { clock : 'a
        ; clear : 'a
        ; data_in_valid : 'a
        ; data_in : 'a [@bits 8]
        }
      [@@deriving sexp_of, hardcaml]
    end

    module O = struct
      type 'a t =
        { data_out_valid : 'a
        ; data_out : 'a [@bits 8]
        ; last : 'a
        ; parity_error : 'a
        ; stop_bit_unstable : 'a
        }
      [@@deriving sexp_of, hardcaml]
    end

    let create (scope : Scope.t) { I.clock; clear; data_in_valid; data_in } =
      let { Uart_tx.O.uart_tx; _ } =
        Uart_tx.hierarchical
          ~instance:"tx"
          scope
          { Uart_tx.I.clock; clear; data_in_valid; data_in }
      in
      let { Uart_rx.O.data_out_valid; data_out; parity_error; stop_bit_unstable } =
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
          ; out = { ready = one 1 }
          }
      in
      { O.data_out_valid = out.valid
      ; data_out = out.data.data
      ; last = out.data.last
      ; parity_error
      ; stop_bit_unstable
      }
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
  inputs.clear := gnd;
  let all_outputs = ref [] in
  List.iter
    ~f:(fun input ->
      inputs.data_in_valid := vdd;
      inputs.data_in := of_int ~width:8 input;
      Cyclesim.cycle sim;
      inputs.data_in_valid := of_int ~width:1 0;
      let rec loop_until_finished acc n =
        if n = 0
        then List.rev acc
        else (
          Cyclesim.cycle sim;
          let acc =
            if Bits.to_bool !(outputs.data_out_valid)
            then Bits.to_int !(outputs.data_out) :: acc
            else acc
          in
          loop_until_finished acc (n - 1))
      in
      (* TODO: Don't just arbitrarily pad, instead wait for a tlast *)
      let outputs = loop_until_finished [] 100 in
      all_outputs := !all_outputs @ outputs)
    all_inputs;
  let output_packet = List.map ~f:Char.of_int_exn !all_outputs |> String.of_char_list in
  print_s [%message "" ~input_packet:packet ~output_packet];
  if debug
  then Waveform.expect ~serialize_to:name ~display_width:150 ~display_height:100 waveform
  else ();
  if not (String.( = ) output_packet packet)
  then raise_s [%message "output packet did not match input"]
;;

let%expect_test "test" =
  test
    ~name:"/tmp/test_serial_to_packet_no_parity_hello_world"
    ~clock_frequency:200
    ~baud_rate:200
    ~include_parity_bit:false
    ~stop_bits:1
    ~packet:"Hello world";
  [%expect {|
     ((input_packet "Hello world") (output_packet "Hello world")) |}];
  test
    ~name:"/tmp/test_serial_to_packet_parity_hello_world"
    ~clock_frequency:200
    ~baud_rate:200
    ~include_parity_bit:true
    ~stop_bits:1
    ~packet:"Hello world";
  [%expect {|
    ((input_packet "Hello world") (output_packet "Hello world")) |}];
  test
    ~name:"/tmp/test_serial_to_packet_no_parity_a"
    ~clock_frequency:200
    ~baud_rate:200
    ~include_parity_bit:false
    ~stop_bits:1
    ~packet:"A";
  [%expect {|
    ((input_packet A) (output_packet A)) |}];
  test
    ~name:"/tmp/test_serial_to_packet_parity_a"
    ~clock_frequency:200
    ~baud_rate:200
    ~include_parity_bit:true
    ~stop_bits:1
    ~packet:"A";
  [%expect {|
    ((input_packet A) (output_packet A)) |}]
;;
