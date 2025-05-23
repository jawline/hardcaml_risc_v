open! Core
open Hardcaml
open Hardcaml_test_harness
module Test_util = Util
open Hardcaml_risc_v_hart
open Hardcaml_memory_controller
open! Bits

let debug = false

module Hart_config = struct
  let register_width = Register_width.B32
  let num_registers = 32
  let design_frequency = 50_000_000_000
end

module Memory_controller = Memory_controller.Make (struct
    let capacity_in_bytes = 16
    let num_read_channels = 1
    let num_write_channels = 1
    let address_width = 32
    let data_bus_width = 32
  end)

open Memory_controller.Memory_bus
module Store = Store.Make (Hart_config) (Memory_controller.Memory_bus)

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
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t =
      { error : 'a
      ; finished : 'a
      ; read_response : 'a Read_response.With_valid.t
      }
    [@@deriving hardcaml]
  end

  let create
        (scope : Scope.t)
        ({ I.clock; clear; enable; funct3; destination; value } : _ I.t)
    =
    let read_bus = Read_bus.Dest.Of_always.wire zero in
    let read_response = Read_response.With_valid.Of_always.wire zero in
    let write_bus = Write_bus.Dest.Of_always.wire zero in
    let write_response = Write_response.With_valid.Of_always.wire zero in
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
        ; read_bus = Read_bus.Dest.Of_always.value read_bus
        ; read_response = Read_response.With_valid.Of_always.value read_response
        ; write_bus = Write_bus.Dest.Of_always.value write_bus
        ; write_response = Write_response.With_valid.Of_always.value write_response
        }
    in
    let controller =
      Memory_controller.hierarchical
        ~priority_mode:Priority_order
        ~request_delay:1
        ~read_latency:1
        scope
        { Memory_controller.I.clock
        ; clear
        ; write_to_controller = [ store.write_bus ]
        ; read_to_controller = [ store.read_bus ]
        }
    in
    compile
      [ Read_bus.Dest.Of_always.assign
          read_bus
          (List.nth_exn controller.read_to_controller 0)
      ; Read_response.With_valid.Of_always.assign
          read_response
          (List.nth_exn controller.read_response 0)
      ; Write_bus.Dest.Of_always.assign
          write_bus
          (List.nth_exn controller.write_to_controller 0)
      ; Write_response.With_valid.Of_always.assign
          write_response
          (List.nth_exn controller.write_response 0)
      ];
    { O.error = store.error
    ; finished = store.finished
    ; read_response = List.nth_exn controller.read_response 0
    }
  ;;
end

module Harness = Cyclesim_harness.Make (Test_machine.I) (Test_machine.O)

let create_sim f =
  Harness.run
    ~trace:`All_named
    ~create:Test_machine.create
    (fun ~inputs:_ ~outputs:_ sim -> f sim)
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
     inputs.destination := of_unsigned_int ~width:32 destination;
     inputs.value := of_unsigned_int ~width:32 value;
     inputs.funct3 := of_unsigned_int ~width:3 funct3;
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
  create_sim (fun sim ->
    (* Aligned store, we expect these to succeed. *)
    test
      ~destination:0
      ~value:0xDEADBEEF
      ~funct3:(Funct3.Store.to_int Funct3.Store.Sw)
      sim;
    [%expect
      {|
    (outputs
     ((error 0) (finished 0)
      (read_response
       ((valid 0)
        (value ((error 0) (read_data 00000000000000000000000000000000)))))))
    deadbeef ffffffff ffffffff ffffffff
    |}];
    (* Unaligned store, we expect no change *)
    test ~destination:1 ~value:0xCC ~funct3:(Funct3.Store.to_int Funct3.Store.Sw) sim;
    [%expect
      {|
    (outputs
     ((error 1) (finished 1)
      (read_response
       ((valid 0)
        (value ((error 0) (read_data 00000000000000000000000000000000)))))))
    ffffffff ffffffff ffffffff ffffffff
    |}];
    (* Aligned store half, we expect these to succeed. *)
    test ~destination:0 ~value:0xABAB ~funct3:(Funct3.Store.to_int Funct3.Store.Sh) sim;
    [%expect
      {|
    (outputs
     ((error 0) (finished 0)
      (read_response
       ((valid 0)
        (value ((error 0) (read_data 11111111111111111111111111111111)))))))
    ffffabab ffffffff ffffffff ffffffff
    |}];
    test ~destination:2 ~value:0xEDAB ~funct3:(Funct3.Store.to_int Funct3.Store.Sh) sim;
    [%expect
      {|
    (outputs
     ((error 0) (finished 0)
      (read_response
       ((valid 0)
        (value ((error 0) (read_data 11111111111111111111111111111111)))))))
    edabffff ffffffff ffffffff ffffffff
    |}];
    (* Test unaligned Sh, we expect these to fail *)
    test ~destination:1 ~value:0xEDAB ~funct3:(Funct3.Store.to_int Funct3.Store.Sh) sim;
    [%expect
      {|
    (outputs
     ((error 1) (finished 1)
      (read_response
       ((valid 0)
        (value ((error 0) (read_data 11111111111111111111111111111111)))))))
    ffffffff ffffffff ffffffff ffffffff
    |}];
    test ~destination:3 ~value:0xEDAB ~funct3:(Funct3.Store.to_int Funct3.Store.Sh) sim;
    [%expect
      {|
    (outputs
     ((error 1) (finished 1)
      (read_response
       ((valid 0)
        (value ((error 0) (read_data 11111111111111111111111111111111)))))))
    ffffffff ffffffff ffffffff ffffffff
    |}];
    (* Test SB, these cannot be unaligned. *)
    test ~destination:0 ~value:0xAA ~funct3:(Funct3.Store.to_int Funct3.Store.Sb) sim;
    [%expect
      {|
    (outputs
     ((error 0) (finished 0)
      (read_response
       ((valid 0)
        (value ((error 0) (read_data 11111111111111111111111111111111)))))))
    ffffffaa ffffffff ffffffff ffffffff
    |}];
    test ~destination:1 ~value:0xAA ~funct3:(Funct3.Store.to_int Funct3.Store.Sb) sim;
    [%expect
      {|
    (outputs
     ((error 0) (finished 0)
      (read_response
       ((valid 0)
        (value ((error 0) (read_data 11111111111111111111111111111111)))))))
    ffffaaff ffffffff ffffffff ffffffff
    |}];
    test ~destination:2 ~value:0xAA ~funct3:(Funct3.Store.to_int Funct3.Store.Sb) sim;
    [%expect
      {|
    (outputs
     ((error 0) (finished 0)
      (read_response
       ((valid 0)
        (value ((error 0) (read_data 11111111111111111111111111111111)))))))
    ffaaffff ffffffff ffffffff ffffffff
    |}];
    test ~destination:3 ~value:0xAA ~funct3:(Funct3.Store.to_int Funct3.Store.Sb) sim;
    [%expect
      {|
    (outputs
     ((error 0) (finished 0)
      (read_response
       ((valid 0)
        (value ((error 0) (read_data 11111111111111111111111111111111)))))))
    aaffffff ffffffff ffffffff ffffffff
    |}];
    [%expect {| |}])
;;
