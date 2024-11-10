open! Core
open Hardcaml
open Hardcaml_waveterm
open Hardcaml_memory_controller
open Hardcaml_framebuffer_expander
open! Bits

module FBC = struct
  let input_width = 3
  let input_height = 3
  let output_width = 8
  let output_height = 8
end

module Memory_controller = Memory_controller.Make (struct
    let capacity_in_bytes = 128
    let num_read_channels = 1
    let num_write_channels = 1
    let address_width = 32
    let data_bus_width = 32
  end)

module Machine = struct
  open Signal
  open Memory_controller.Memory_bus

  module Framebuffer_expander =
    Framebuffer_expander.Make (FBC) (Memory_controller.Memory_bus)

  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; start_frame : 'a
      ; next_pixel : 'a
      }
    [@@deriving hardcaml ~rtlmangle:"$"]
  end

  module O = struct
    type 'a t = { pixel : 'a } [@@deriving hardcaml ~rtlmangle:"$"]
  end

  let create (scope : Scope.t) { I.clock; clear; start_frame; next_pixel } =
    let request_ack = Read_bus.Rx.Of_signal.wires () in
    let response = Read_response.With_valid.Of_signal.wires () in
    let frame =
      Framebuffer_expander.hierarchical
        scope
        { Framebuffer_expander.I.clock
        ; clear
        ; start = start_frame
        ; start_address = of_int ~width:32 0
        ; next = next_pixel
        ; memory_request = request_ack
        ; memory_response = response
        }
    in
    let controller =
      Memory_controller.hierarchical
        ~instance:"memory_controller"
        scope
        { Memory_controller.I.clock
        ; clear
        ; read_to_controller = [ frame.memory_request ]
        ; write_to_controller = [ Write_bus.Tx.Of_signal.of_int 0 ]
        }
    in
    Read_bus.Rx.Of_signal.(request_ack <== List.hd_exn controller.read_to_controller);
    Read_response.With_valid.Of_signal.(response <== List.hd_exn controller.read_response);
    { O.pixel = frame.pixel }
  ;;
end

let debug = true

let program_ram sim bits =
  let ram = Cyclesim.lookup_mem_by_name sim "main_memory_bram" |> Option.value_exn in
  Array.iteri ~f:(fun i m -> Cyclesim.Memory.of_bits ~address:i ram m) bits
;;

let test ~name ~framebuffer =
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
  program_ram sim framebuffer;
  inputs.clear := vdd;
  Cyclesim.cycle sim;
  Cyclesim.cycle sim;
  Cyclesim.cycle sim;
  inputs.clear := gnd;
  let wait_some_cycles_and_sample () =
    Sequence.range 0 10 |> Sequence.iter ~f:(fun _ -> Cyclesim.cycle sim);
    let result = Bits.to_bool !(outputs.pixel) in
    inputs.next_pixel := vdd;
    Cyclesim.cycle sim;
    inputs.next_pixel := gnd;
    result
  in
  inputs.start_frame := vdd;
  Cyclesim.cycle sim;
  inputs.start_frame := gnd;
  let frame_buffer =
    Array.init
      ~f:(fun _ -> wait_some_cycles_and_sample ())
      (FBC.output_width * FBC.output_height)
  in
  Sequence.range 0 FBC.output_height
  |> Sequence.iter ~f:(fun y ->
    Sequence.range 0 FBC.output_width
    |> Sequence.iter ~f:(fun x ->
      let px = Array.get frame_buffer ((y * FBC.output_width) + x) in
      if px then printf "*" else printf "-");
    printf "\n");
  if debug then Waveform.Serialize.marshall waveform name
;;

let%expect_test "details" =
  print_s
    [%message
      (Machine.Framebuffer_expander.scaling_factor_y : int)
        (Machine.Framebuffer_expander.scaling_factor_x : int)
        (Machine.Framebuffer_expander.margin_x_start : int)
        (Machine.Framebuffer_expander.margin_x_end : int)
        (Machine.Framebuffer_expander.margin_y_start : int)
        (Machine.Framebuffer_expander.margin_y_end : int)];
  [%expect
    {|
    ((Machine.Framebuffer_expander.scaling_factor_y 2)
     (Machine.Framebuffer_expander.scaling_factor_x 2)
     (Machine.Framebuffer_expander.margin_x_start 1)
     (Machine.Framebuffer_expander.margin_x_end 1)
     (Machine.Framebuffer_expander.margin_y_start 1)
     (Machine.Framebuffer_expander.margin_y_end 1))
    |}]
;;

let%expect_test "test" =
  let strided = Bits.of_int ~width:32 0b0101 in
  let rev_strided = Bits.of_int ~width:32 0b1010 in
  test ~name:"/tmp/test_framebuffer_expander" ~framebuffer:[| strided ; rev_strided ; strided |];
  [%expect
    {|
    --------
    -**--**-
    -**--**-
    ---**---
    ---**---
    -**--**-
    -**--**-
    --------
    |}]
;;
