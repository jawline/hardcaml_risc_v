open! Core
open Hardcaml
open Hardcaml_waveterm
open Hardcaml_uart_controller
open Hardcaml_io_controller
open! Bits

let debug = true

let test ~name ~clock_frequency ~baud_rate ~include_parity_bit ~stop_bits ~packets =
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
      [@@deriving hardcaml]
    end

    module O = struct
      type 'a t =
        { data_out_valid : 'a
        ; data_out : 'a [@bits 8]
        ; last : 'a
        ; parity_error : 'a
        ; tx_idle : 'a
        }
      [@@deriving hardcaml]
    end

    let create (scope : Scope.t) { I.clock; clear; data_in_valid; data_in } =
      let { Uart_tx.O.uart_tx; idle = tx_idle; _ } =
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
      let { Serial_to_packet.O.out; ready = _ } =
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
      { O.data_out_valid = out.valid
      ; data_out = out.data.data
      ; last = out.data.last
      ; parity_error
      ; tx_idle
      }
    ;;
  end
  in
  let module Tb = Hardcaml_step_testbench.Functional.Cyclesim.Make (Machine.I) (Machine.O)
  in
  let open Tb.Let_syntax in
  let create_sim () =
    let module Sim = Cyclesim.With_interface (Machine.I) (Machine.O) in
    Sim.create
      ~config:Cyclesim.Config.trace_all
      (Machine.create
         (Scope.create ~auto_label_hierarchical_ports:true ~flatten_design:true ()))
  in
  let sim = create_sim () in
  let waveform, sim = Waveform.create sim in
  let rec send_inputs = function
    | [] -> return ()
    | packet :: xs ->
      let input_bytes =
        (* We add the header and then the packet length before the packet *)
        let packet = String.to_list packet in
        let packet_size = List.length packet in
        let packet_len_msb = packet_size land 0xFF00 in
        let packet_len_lsb = packet_size land 0x00FF in
        [ Char.to_int 'Q'; packet_len_msb; packet_len_lsb ]
        @ List.map ~f:Char.to_int packet
      in
      let rec consume_bytes = function
        | [] -> return ()
        | x :: xs ->
          let%bind () =
            Tb.cycle
              { Tb.input_zero with data_in_valid = vdd; data_in = Bits.of_int ~width:8 x }
            >>| ignore
          in

          let rec wait_until_idle () = 
            let%bind result = Tb.cycle Tb.input_zero   in
            if Bits.to_bool result.after_edge.tx_idle then return () else wait_until_idle () in
          let%bind () = wait_until_idle () in
          consume_bytes xs 

      in
      let%bind () = consume_bytes input_bytes in
      send_inputs xs
  in
  let receive_packet () =
    let output_bytes = ref [] in
    let cycles = ref 0 in
    let rec loop () =
      let%bind output = Tb.cycle Tb.input_hold in
      incr cycles;
      let output = output.before_edge in
      if Bits.to_bool output.data_out_valid
      then output_bytes := Bits.to_int output.data_out :: !output_bytes;
      if Bits.to_bool output.data_out_valid && Bits.to_bool output.last then return (List.rev !output_bytes |> List.map ~f:(Char.of_int_exn) |> String.of_char_list ) else loop ()
    in
    let%bind result = loop () in
    print_s [%message "Received packet in" ~cycles:(!cycles : int)];
    Tb.return result
  in
  let receive_packets ~n =
    let rec inner ~n =
      if n = 0
      then return []
      else (
        let%bind next = receive_packet () in
        print_s [%message "Next packet" (next : string)];
        let%bind succ = inner ~n:(n - 1) in
        return ( next :: succ))
    in
     inner ~n 
  in
    

  let testbench _i =
    let%bind () = Tb.cycle { Tb.input_zero with clear = vdd } >>| ignore in
    let%bind result = Tb.spawn (fun _ ->
receive_packets ~n:(List.length packets)) in

    let%bind () = send_inputs packets in
    let%bind result = Tb.wait_for result in
    if debug then Waveform.Serialize.marshall waveform name;


    if not (List.equal   String.equal result packets) then raise_s [%message "BUG: Inputs and outputs diverge" (packets : string list) (result : string list) ];

    print_s [%message "PASSED" ];

    return () 
    
  in
  Option.value_exn (Tb.run_with_timeout ~timeout:5000 ~simulator:sim ~testbench ())
;;

let%expect_test "test" =
  test
    ~name:"/tmp/test_serial_to_packet_no_parity_hello_world"
    ~clock_frequency:200
    ~baud_rate:50
    ~include_parity_bit:false
    ~stop_bits:1
    ~packets:[ "Hello world"; "Goodbye world"; "Yet another packet!!!!!!" ];
  [%expect
    {|
    ("Received packet in" (cycles 575))
    ("Next packet" (next "Hello world"))
    ("Received packet in" (cycles 656))
    ("Next packet" (next "Goodbye world"))
    ("Received packet in" (cycles 1107))
    ("Next packet" (next "Yet another packet!!!!!!"))
    PASSED
    |}];
  test
    ~name:"/tmp/test_serial_to_packet_parity_hello_world"
    ~clock_frequency:200
    ~baud_rate:50
    ~include_parity_bit:true
    ~stop_bits:1
    ~packets:[ "Hello world"; "Goodbye world"; "Yet another packet!!!!!!" ];
  [%expect
    {|
    ("Received packet in" (cycles 631))
    ("Next packet" (next "Hello world"))
    ("Received packet in" (cycles 720))
    ("Next packet" (next "Goodbye world"))
    ("Received packet in" (cycles 1215))
    ("Next packet" (next "Yet another packet!!!!!!"))
    PASSED
    |}];
  test
    ~name:"/tmp/test_serial_to_packet_no_parity_a"
    ~clock_frequency:200
    ~baud_rate:50
    ~include_parity_bit:false
    ~stop_bits:1
    ~packets:[ "Hello world"; "Goodbye world"; "Yet another packet!!!!!!" ];
  [%expect
    {|
    ("Received packet in" (cycles 575))
    ("Next packet" (next "Hello world"))
    ("Received packet in" (cycles 656))
    ("Next packet" (next "Goodbye world"))
    ("Received packet in" (cycles 1107))
    ("Next packet" (next "Yet another packet!!!!!!"))
    PASSED
    |}];
  test
    ~name:"/tmp/test_serial_to_packet_parity_a"
    ~clock_frequency:200
    ~baud_rate:50
    ~include_parity_bit:true
    ~stop_bits:1
    ~packets:[ "Hello world"; "Goodbye world"; "Yet another packet!!!!!!" ];
  [%expect
    {|
    ("Received packet in" (cycles 631))
    ("Next packet" (next "Hello world"))
    ("Received packet in" (cycles 720))
    ("Next packet" (next "Goodbye world"))
    ("Received packet in" (cycles 1215))
    ("Next packet" (next "Yet another packet!!!!!!"))
    PASSED
    |}]
;;
