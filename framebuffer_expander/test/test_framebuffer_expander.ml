open! Core
open Hardcaml
open Hardcaml_waveterm
open Hardcaml_memory_controller
open Hardcaml_framebuffer_expander
open! Bits


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
    Framebuffer_expander.Make
      (struct
        let input_width = 32
        let input_height = 32
        let output_width = 67
        let output_height = 33
      end)
      (Memory_controller.Memory_bus)

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
        ; start_address = of_int ~width:32 8
        ; next = next_pixel ;
        memory_request = request_ack
        ; memory_response = response
        }
    in
    let controller =
      Memory_controller.hierarchical
        ~instance:"memory_controller"
        scope
        { Memory_controller.I.clock
        ; clear
        ; read_to_controller = 
[ frame.memory_request ]

        ; write_to_controller = [ Write_bus.Tx.Of_signal.of_int 0 ] 
        }
    in
    Read_bus.Rx.Of_signal.(request_ack <== List.hd_exn controller.read_to_controller );
    Read_response.With_valid.Of_signal.(response <== List.hd_exn controller.read_response );
    { O.pixel = frame.pixel }
  ;;
end

let debug = false

let test ~name =
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
  let _outputs : _ Machine.O.t = Cyclesim.outputs sim in
  (* The fifo needs a clear cycle to initialize *)
  inputs.clear := vdd;
  Cyclesim.cycle sim;
  Cyclesim.cycle sim;
  Cyclesim.cycle sim;
  inputs.clear := gnd;
    if debug then Waveform.Serialize.marshall waveform name
;;

let%expect_test "test" =
  test
    ~name:"/tmp/test_framebuffer_expander";
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)
  "Assert_failure framebuffer_expander/lib/framebuffer_expander.ml:137:28"
  Raised at Hardcaml_framebuffer_expander__Framebuffer_expander.Make.create in file "framebuffer_expander/lib/framebuffer_expander.ml", line 137, characters 28-40
  Called from Hardcaml__Hierarchy.In_scope.create in file "src/hierarchy.ml", line 105, characters 18-40
  Called from Hardcaml_framebuffer_expander_test__Test_framebuffer_expander.Machine.create in file "framebuffer_expander/test/test_framebuffer_expander.ml", line 49, characters 6-297
  Called from Hardcaml__Circuit.With_interface.create_exn in file "src/circuit.ml", line 424, characters 18-34
  Called from Hardcaml__Cyclesim.With_interface.create in file "src/cyclesim.ml", line 146, characters 18-81
  Called from Hardcaml_framebuffer_expander_test__Test_framebuffer_expander.test.create_sim in file "framebuffer_expander/test/test_framebuffer_expander.ml", line 83, characters 4-161
  Called from Hardcaml_framebuffer_expander_test__Test_framebuffer_expander.(fun) in file "framebuffer_expander/test/test_framebuffer_expander.ml", line 102, characters 2-49
  Called from Ppx_expect_runtime__Test_block.Configured.dump_backtrace in file "runtime/test_block.ml", line 142, characters 10-28
  |}]
;;
