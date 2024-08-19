open! Core
open Hardcaml
module Test_util = Util
open Hardcaml_waveterm
open Hardcaml_risc_v_hart
open Hardcaml_memory_controller
open! Bits

let debug = false

module Hart_config = struct
  let register_width = Register_width.B32
  let num_registers = 32
end

module Memory_controller = Memory_controller.Make (struct
    let capacity_in_bytes = 16
    let num_channels = 1
    let address_width = 32
    let data_bus_width = 32
  end)

module Store = Store.Make (Hart_config) (Memory_controller)

module Test_machine = struct
  open! Signal
  open! Always

  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; enable : 'a
      ; funct3 : 'a [@bits 3]
      ; destination : 'a [@bits 32]
      ; value : 'a [@bits 32]
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t =
      { error : 'a
      ; finished : 'a
      ; controller_to_hart : 'a Memory_controller.Rx_bus.Tx.t
           [@rtlprefix "controller_to_hart"]
      ; hart_to_memory_controller : 'a Memory_controller.Tx_bus.Rx.t
           [@rtlprefix "hart_to_controller"]
      }
    [@@deriving sexp_of, hardcaml]
  end

  let create
    (scope : Scope.t)
    ({ I.clock; clear; enable; funct3; destination; value } : _ I.t)
    =
    let memory_controller_to_hart = Memory_controller.Rx_bus.Tx.Of_always.wire zero in
    let hart_to_memory_controller = Memory_controller.Tx_bus.Rx.Of_always.wire zero in
    let store =
      Store.hierarchical
        ~instance:"store"
        scope
        { Store.I.clock
        ; clear
        ; enable
        ; funct3
        ; destination
        ; value
        ; memory_controller_to_hart =
            Memory_controller.Rx_bus.Tx.Of_always.value memory_controller_to_hart
        ; hart_to_memory_controller =
            Memory_controller.Tx_bus.Rx.Of_always.value hart_to_memory_controller
        }
    in
    let controller =
      Memory_controller.hierarchical
        ~instance:"memory_controller"
        scope
        { Memory_controller.I.clock
        ; clear
        ; ch_to_controller = [ store.hart_to_memory_controller ]
        }
    in
    compile
      [ Memory_controller.Rx_bus.Tx.Of_always.assign
          memory_controller_to_hart
          (List.nth_exn controller.controller_to_ch 0)
      ; Memory_controller.Tx_bus.Rx.Of_always.assign
          hart_to_memory_controller
          (List.nth_exn controller.ch_to_controller 0)
      ];
    { O.error = store.error
    ; O.finished = store.finished
    ; controller_to_hart =
        Memory_controller.Rx_bus.Tx.Of_always.value memory_controller_to_hart
    ; hart_to_memory_controller =
        Memory_controller.Tx_bus.Rx.Of_always.value hart_to_memory_controller
    }
  ;;
end

let create_sim () =
  let module Sim = Cyclesim.With_interface (Test_machine.I) (Test_machine.O) in
  Sim.create
    ~config:Cyclesim.Config.trace_all
    (Test_machine.create
       (Scope.create ~auto_label_hierarchical_ports:true ~flatten_design:true ()))
;;

let test ~destination ~value ~funct3 sim =
  (* Initialize the main memory to some known values for testing. *)
  Test_util.program_ram sim (Array.init ~f:(Fn.const (ones 32)) 4);
  (try
     let inputs : _ Test_machine.I.t = Cyclesim.inputs sim in
     let outputs_before : _ Test_machine.O.t =
       Cyclesim.outputs ~clock_edge:Side.Before sim
     in
     let outputs : _ Test_machine.O.t = Cyclesim.outputs sim in
     inputs.enable := Bits.vdd;
     inputs.destination := of_int ~width:32 destination;
     inputs.value := of_int ~width:32 value;
     inputs.funct3 := of_int ~width:3 funct3;
     let rec loop_until_finished max =
       if max = 0 then raise_s [%message "BUG: Timed out"];
       Cyclesim.cycle sim;
       if Bits.to_bool !(outputs_before.finished)
       then ()
       else loop_until_finished (max - 1)
     in
     loop_until_finished 50;
     print_s [%message (outputs : Bits.t ref Test_machine.O.t)]
   with
   | _ -> print_s [%message "BUG: Timed out"]);
  Test_util.print_ram sim
;;

let%expect_test "store" =
  let sim = create_sim () in
  let waveform, sim = Waveform.create sim in
  (* Aligned store, we expect these to succeed. *)
  test ~destination:0 ~value:0xDEADBEEF ~funct3:(Funct3.Store.to_int Funct3.Store.Sw) sim;
  [%expect
    {|
    (outputs
     ((error 0) (finished 0)
      (controller_to_hart
       ((valid 0)
        (data ((error 0) (read_data 00000000000000000000000000000000)))))
      (hart_to_memory_controller ((ready 1)))))
    deadbeef ffffffff ffffffff ffffffff
    |}];
  (* Unaligned store, we expect no change *)
  test ~destination:1 ~value:0xCC ~funct3:(Funct3.Store.to_int Funct3.Store.Sw) sim;
  [%expect
    {|
     (outputs
      ((error 1) (finished 1)
       (controller_to_hart
        ((valid 0)
         (data ((error 0) (read_data 00000000000000000000000000000000)))))
       (hart_to_memory_controller ((ready 1)))))
     ffffffff ffffffff ffffffff ffffffff |}];
  (* Aligned store half, we expect these to succeed. *)
  test ~destination:0 ~value:0xABAB ~funct3:(Funct3.Store.to_int Funct3.Store.Sh) sim;
  [%expect
    {|
     (outputs
      ((error 0) (finished 0)
       (controller_to_hart
        ((valid 0)
         (data ((error 0) (read_data 11111111111111111111111111111111)))))
       (hart_to_memory_controller ((ready 1)))))
     ffffabab ffffffff ffffffff ffffffff |}];
  test ~destination:2 ~value:0xEDAB ~funct3:(Funct3.Store.to_int Funct3.Store.Sh) sim;
  [%expect
    {|
     (outputs
      ((error 0) (finished 0)
       (controller_to_hart
        ((valid 0)
         (data ((error 0) (read_data 11111111111111111111111111111111)))))
       (hart_to_memory_controller ((ready 1)))))
     edabffff ffffffff ffffffff ffffffff |}];
  (* Test unaligned Sh, we expect these to fail *)
  test ~destination:1 ~value:0xEDAB ~funct3:(Funct3.Store.to_int Funct3.Store.Sh) sim;
  [%expect
    {|
     (outputs
      ((error 1) (finished 1)
       (controller_to_hart
        ((valid 0)
         (data ((error 0) (read_data 11111111111111111111111111111111)))))
       (hart_to_memory_controller ((ready 1)))))
     ffffffff ffffffff ffffffff ffffffff |}];
  test ~destination:3 ~value:0xEDAB ~funct3:(Funct3.Store.to_int Funct3.Store.Sh) sim;
  [%expect
    {|
     (outputs
      ((error 1) (finished 1)
       (controller_to_hart
        ((valid 0)
         (data ((error 0) (read_data 11111111111111111111111111111111)))))
       (hart_to_memory_controller ((ready 1)))))
     ffffffff ffffffff ffffffff ffffffff |}];
  (* Test SB, these cannot be unaligned. *)
  test ~destination:0 ~value:0xAA ~funct3:(Funct3.Store.to_int Funct3.Store.Sb) sim;
  [%expect
    {|
     (outputs
      ((error 0) (finished 0)
       (controller_to_hart
        ((valid 0)
         (data ((error 0) (read_data 11111111111111111111111111111111)))))
       (hart_to_memory_controller ((ready 1)))))
     ffffffaa ffffffff ffffffff ffffffff |}];
  test ~destination:1 ~value:0xAA ~funct3:(Funct3.Store.to_int Funct3.Store.Sb) sim;
  [%expect
    {|
     (outputs
      ((error 0) (finished 0)
       (controller_to_hart
        ((valid 0)
         (data ((error 0) (read_data 11111111111111111111111111111111)))))
       (hart_to_memory_controller ((ready 1)))))
     ffffaaff ffffffff ffffffff ffffffff |}];
  test ~destination:2 ~value:0xAA ~funct3:(Funct3.Store.to_int Funct3.Store.Sb) sim;
  [%expect
    {|
     (outputs
      ((error 0) (finished 0)
       (controller_to_hart
        ((valid 0)
         (data ((error 0) (read_data 11111111111111111111111111111111)))))
       (hart_to_memory_controller ((ready 1)))))
     ffaaffff ffffffff ffffffff ffffffff |}];
  test ~destination:3 ~value:0xAA ~funct3:(Funct3.Store.to_int Funct3.Store.Sb) sim;
  [%expect
    {|
     (outputs
      ((error 0) (finished 0)
       (controller_to_hart
        ((valid 0)
         (data ((error 0) (read_data 11111111111111111111111111111111)))))
       (hart_to_memory_controller ((ready 1)))))
     aaffffff ffffffff ffffffff ffffffff |}];
  if debug then Waveform.Serialize.marshall waveform "/tmp/test_store";
  [%expect {| |}]
;;
