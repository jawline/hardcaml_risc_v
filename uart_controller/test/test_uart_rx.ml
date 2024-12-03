open! Core
open Hardcaml
open Hardcaml_waveterm
open Hardcaml_uart_controller
open! Bits

let debug = true

let test ~name ~clock_frequency ~baud_rate ~include_parity_bit ~stop_bits ~all_inputs =
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
  let module Uart_tx = Uart_tx.Make (Config) in
  let module Uart_rx = Uart_rx.Make (Config) in
  let module Machine = struct
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
        { data_out_valid : 'a
        ; data_out : 'a [@bits 8]
        ; parity_error : 'a
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
      let { Uart_rx.O.data_out_valid; data_out; parity_error } =
        Uart_rx.hierarchical
          ~instance:"rx"
          scope
          { Uart_rx.I.clock; clear; uart_rx = uart_tx }
      in
      { O.data_out_valid; data_out; parity_error }
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
  inputs.clear := vdd;
  Cyclesim.cycle sim;
  inputs.clear := gnd;
  Cyclesim.cycle sim;
  let all_outputs = ref [] in
  let total_cycles = ref 0 in
  List.iter
    ~f:(fun input ->
      inputs.data_in_valid := of_int ~width:1 1;
      inputs.data_in := of_int ~width:8 input;
      Cyclesim.cycle sim;
      incr total_cycles;
      inputs.data_in_valid := of_int ~width:1 0;
      let rec loop_until_finished acc n =
        if n = 0
        then List.rev acc
        else (
          Cyclesim.cycle sim;
          incr total_cycles;
          let acc =
            if Bits.to_bool !(outputs.data_out_valid)
               || Bits.to_bool !(outputs.parity_error)
            then Bits.to_int !(outputs.data_out) :: acc
            else acc
          in
          loop_until_finished acc (n - 1))
      in
      let cycles_per_bit = clock_frequency / baud_rate in
      let number_of_cycles_to_loop_for =
        (cycles_per_bit * (1 + 8 + (if include_parity_bit then 1 else 0) + stop_bits))
        + cycles_per_bit
      in
      let outputs = loop_until_finished [] number_of_cycles_to_loop_for in
      all_outputs := !all_outputs @ outputs)
    all_inputs;
  print_s [%message "" ~_:(!all_outputs : int list) ~_:(!total_cycles : int)];
  if debug then Waveform.Serialize.marshall waveform name else ();
  if not (List.equal Int.( = ) !all_outputs all_inputs)
  then raise_s [%message "outputs did not match inputs"]
;;

let%expect_test "test" =
  test
    ~name:"/tmp/one_stop_bit_no_parity"
    ~clock_frequency:10
    ~baud_rate:1
    ~include_parity_bit:false
    ~stop_bits:1
    ~all_inputs:[ 255; 0; 10; 7; 153; 170 ];
  [%expect {| ((255 0 10 7 153 170) 666) |}];
  test
    ~name:"/tmp/one_stop_bit_with_parity"
    ~clock_frequency:200
    ~baud_rate:50
    ~include_parity_bit:true
    ~stop_bits:1
    ~all_inputs:[ 0b1010; 0b111; 0b1001_1001; 0b1111_1111; 0b0000_0000; 0b1010_1010 ];
  [%expect {| ((10 7 153 255 0 170) 294) |}];
  test
    ~name:"/tmp/two_stop_bits_with_parity"
    ~clock_frequency:200
    ~baud_rate:50
    ~include_parity_bit:true
    ~stop_bits:2
    ~all_inputs:[ 0b1010; 0b111; 0b1001_1001; 0b1111_1111; 0b0000_0000; 0b1010_1010 ];
  [%expect {| ((10 7 153 255 0 170) 318) |}]
;;
