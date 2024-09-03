open! Core
open Hardcaml
open Hardcaml_risc_v
open Hardcaml_risc_v_hart
open Hardcaml_uart_controller
open Hardcaml_waveterm
open Opcode_helper
open! Bits

let debug = false

module Make (M : sig
    type sim

    val create_sim : string -> sim
    val finalize_sim : sim -> unit
    val test_and_registers : instructions:Bits.t list -> sim -> int * int list
    val test : instructions:Bits.t list -> sim -> unit
    val print_ram : sim -> unit
  end) =
struct
  let create_sim = M.create_sim
  let finalize_sim = M.finalize_sim
  let test = M.test

  (* TODO: Add qcheck tests for every opcode *)

  let opi_helper ~name ~f ~funct3 ~small_imm_range =
    let open Quickcheck.Generator in
    let sim = create_sim name in
    Quickcheck.test
      ~trials:25
      (tuple4
         (Int.gen_incl 1 31)
         (Int.gen_incl 1 31)
         (Int.gen_incl (-2047) 2047)
         (Int.gen_incl
            (if small_imm_range then 0 else -2047)
            (if small_imm_range then 10 else 2047)))
      ~f:(fun (rd, rs1, rs1_initial, imm) ->
        let _pc, registers =
          M.test_and_registers
            ~instructions:
              [ op_imm ~funct3:Funct3.Op.Add_or_sub ~rs1:0 ~rd:rs1 ~immediate:rs1_initial
              ; op_imm ~funct3 ~rs1 ~rd ~immediate:imm
              ]
            sim
        in
        (* TODO: We mask by 4 bytes because OCaml will use 63-bits here. Comparing the Bits.t would probably be better. *)
        let result = List.nth_exn registers rd land 0xFFFFFFFF in
        let expectation = f rs1_initial imm land 0xFFFFFFFF in
        if result <> expectation
        then
          raise_s
            [%message
              "Failed"
                (result : int)
                (expectation : int)
                (rd : int)
                (rs1 : int)
                (rs1_initial : int)
                (imm : int)
                (registers : int list)];
        ())
  ;;

  let%expect_test "addi" =
    opi_helper
      ~name:"addi_qcheck"
      ~funct3:Funct3.Op.Add_or_sub
      ~f:( + )
      ~small_imm_range:false;
    [%expect {| |}]
  ;;

  let%expect_test "and" =
    opi_helper ~name:"and_qcheck" ~funct3:Funct3.Op.And ~f:( land ) ~small_imm_range:false;
    [%expect {| |}]
  ;;

  let%expect_test "xor" =
    opi_helper ~name:"not_qcheck" ~funct3:Funct3.Op.Xor ~f:( lxor ) ~small_imm_range:false;
    [%expect {| |}]
  ;;

  let%expect_test "or" =
    opi_helper ~name:"or_qcheck" ~funct3:Funct3.Op.Or ~f:( lor ) ~small_imm_range:false;
    [%expect {| |}]
  ;;

  let%expect_test "slt" =
    opi_helper
      ~name:"slt_qcheck"
      ~funct3:Funct3.Op.Slt
      ~f:(fun l r -> if l < r then 1 else 0)
      ~small_imm_range:false;
    [%expect {| |}]
  ;;

  let%expect_test "sltu" =
    opi_helper
      ~name:"sltu_qcheck"
      ~funct3:Funct3.Op.Sltu
      ~f:(fun l r -> if l land 0xFFFFFFFF < r land 0xFFFFFFFF then 1 else 0)
      ~small_imm_range:false;
    [%expect {| |}]
  ;;

  let op_helper ~name ~f ~funct3 ~funct7 ~small_rs2_range =
    let sim = create_sim name in
    let open Quickcheck.Generator in
    Core.protect
      ~finally:(fun () -> M.finalize_sim sim)
      ~f:(fun () ->
        Quickcheck.test
          ~trials:25
          (tuple5
             (Int.gen_incl 1 31)
             (Int.gen_incl 1 31)
             (Int.gen_incl 1 31)
             (Int.gen_incl (-2047) 2047)
             (Int.gen_incl
                (if small_rs2_range then 0 else -2047)
                (if small_rs2_range then 10 else 2047)))
          ~f:(fun (rd, rs1, rs2, rs1_initial, rs2_initial) ->
            if rs1 <> rs2
            then (
              let _pc, registers =
                M.test_and_registers
                  ~instructions:
                    [ op_imm
                        ~funct3:Funct3.Op.Add_or_sub
                        ~rs1:0
                        ~rd:rs1
                        ~immediate:rs1_initial
                    ; op_imm
                        ~funct3:Funct3.Op.Add_or_sub
                        ~rs1:0
                        ~rd:rs2
                        ~immediate:rs2_initial
                    ; op ~rs1 ~rs2 ~rd ~funct3 ~funct7
                    ]
                  sim
              in
              let result = List.nth_exn registers rd land 0xFFFFFFFF in
              let expectation = f rs1_initial rs2_initial land 0xFFFFFFFF in
              if result <> expectation
              then
                raise_s
                  [%message
                    "Failed"
                      (result : int)
                      (expectation : int)
                      (rd : int)
                      (rs1 : int)
                      (rs2 : int)
                      (rs1_initial : int)
                      (rs2_initial : int)
                      (registers : int list)])
            else ()))
  ;;

  let%expect_test "add" =
    op_helper
      ~name:"add_qcheck"
      ~funct3:Funct3.Op.Add_or_sub
      ~funct7:0
      ~f:( + )
      ~small_rs2_range:false;
    [%expect {| |}]
  ;;

  let%expect_test "sub" =
    op_helper
      ~name:"sub_qcheck"
      ~funct3:Funct3.Op.Add_or_sub
      ~funct7:0b0100_000
      ~f:( - )
      ~small_rs2_range:false;
    [%expect {| |}]
  ;;

  let%expect_test "and" =
    op_helper
      ~name:"and_qcheck"
      ~funct3:Funct3.Op.And
      ~funct7:0
      ~f:( land )
      ~small_rs2_range:false;
    [%expect {| |}]
  ;;

  let%expect_test "xor" =
    op_helper
      ~name:"not_qcheck"
      ~funct3:Funct3.Op.Xor
      ~funct7:0
      ~f:( lxor )
      ~small_rs2_range:false;
    [%expect {| |}]
  ;;

  let%expect_test "or" =
    op_helper
      ~name:"or_qcheck"
      ~funct3:Funct3.Op.Or
      ~funct7:0
      ~f:( lor )
      ~small_rs2_range:false;
    [%expect {| |}]
  ;;

  let%expect_test "slt" =
    op_helper
      ~name:"slt_qcheck"
      ~funct3:Funct3.Op.Slt
      ~funct7:0
      ~f:(fun l r -> if l < r then 1 else 0)
      ~small_rs2_range:false;
    [%expect {| |}]
  ;;

  let%expect_test "sltu" =
    op_helper
      ~name:"sltu_qcheck"
      ~funct3:Funct3.Op.Sltu
      ~funct7:0
      ~f:(fun l r -> if l land 0xFFFFFFFF < r land 0xFFFFFFFF then 1 else 0)
      ~small_rs2_range:false;
    [%expect {| |}]
  ;;

  let%expect_test "sll" =
    op_helper
      ~name:"sll_qcheck"
      ~funct3:Funct3.Op.Sll
      ~funct7:0
      ~f:(fun l r -> (l land 0xFFFFFFFF) lsl (r land 0xFFFFFFFF))
      ~small_rs2_range:true;
    [%expect {| |}]
  ;;

  let%expect_test "srl" =
    op_helper
      ~name:"srl_qcheck"
      ~funct3:Funct3.Op.Srl_or_sra
      ~funct7:0
      ~f:(fun l r -> (l land 0xFFFFFFFF) lsr (r land 0xFFFFFFFF))
      ~small_rs2_range:true;
    [%expect {| |}]
  ;;

  let%expect_test "sra" =
    op_helper
      ~name:"sra_qcheck"
      ~funct3:Funct3.Op.Srl_or_sra
      ~funct7:0b0100_000
      ~f:(fun (l : int) (r : int) ->
        let shift_right = Int32.shift_right (Int32.of_int_exn l) r in
        Int32.to_int_exn shift_right)
      ~small_rs2_range:true;
    [%expect {| |}]
  ;;

  let branch_helper ~name ~f ~funct3 =
    let sim = create_sim name in
    let open Quickcheck.Generator in
    Quickcheck.test
      ~trials:25
      (tuple4
         (Int.gen_incl 1 31)
         (Int.gen_incl 1 31)
         (Int.gen_incl (-2047) 2047)
         (Int.gen_incl (-2047) 2047))
      ~f:(fun (rs1, rs2, rs1_initial, rs2_initial) ->
        if rs1 <> rs2
        then (
          let pc, registers =
            M.test_and_registers
              ~instructions:
                [ op_imm
                    ~funct3:Funct3.Op.Add_or_sub
                    ~rs1:0
                    ~rd:rs1
                    ~immediate:rs1_initial
                ; op_imm
                    ~funct3:Funct3.Op.Add_or_sub
                    ~rs1:0
                    ~rd:rs2
                    ~immediate:rs2_initial
                ; branch ~rs1 ~rs2 ~funct3 ~offset:250
                ]
              sim
          in
          let result = pc in
          let expectation = if f rs1_initial rs2_initial then 250 + 8 else 12 in
          if result <> expectation
          then
            raise_s
              [%message
                "Failed"
                  (result : int)
                  (expectation : int)
                  (rs1 : int)
                  (rs2 : int)
                  (rs1_initial : int)
                  (rs2_initial : int)
                  (registers : int list)])
        else ())
  ;;

  let%expect_test "beq" =
    branch_helper ~name:"beq_qcheck" ~funct3:Funct3.Branch.Beq ~f:( = );
    [%expect {| |}]
  ;;

  let%expect_test "bne" =
    branch_helper ~name:"bne_qcheck" ~funct3:Funct3.Branch.Bne ~f:( <> );
    [%expect {| |}]
  ;;

  let%expect_test "blt" =
    branch_helper ~name:"blt_qcheck" ~funct3:Funct3.Branch.Blt ~f:( < );
    [%expect {| |}]
  ;;

  let%expect_test "bge" =
    branch_helper ~name:"bge_qcheck" ~funct3:Funct3.Branch.Bge ~f:( >= );
    [%expect {| |}]
  ;;

  let%expect_test "jal" =
    let sim = create_sim "jal" in
    let open Quickcheck.Generator in
    Core.protect
      ~finally:(fun () -> M.finalize_sim sim)
      ~f:(fun () ->
        Quickcheck.test
          ~trials:200
          (tuple2 (Int.gen_incl 1 31) (Int.gen_incl (-65535) 65535))
          ~f:(fun (rd, offset) ->
            let pc, registers =
              M.test_and_registers ~instructions:[ jal ~rd ~offset ] sim
            in
            M.finalize_sim sim;
            let result = pc land 0xFFFFFFFF in
            let rd = List.nth_exn registers rd in
            let expectation = offset land 0xFFFFFFFE in
            if result <> expectation || if rd <> 0 then rd <> 4 else false
            then
              raise_s
                [%message
                  "Failed"
                    (result : int)
                    (expectation : int)
                    (pc : int)
                    (rd : int)
                    (offset : int)
                    (registers : int list)]));
    [%expect {| |}]
  ;;

  let%expect_test "jalr" =
    let sim = create_sim "jalr" in
    let open Quickcheck.Generator in
    Quickcheck.test
      ~trials:25
      (tuple4
         (Int.gen_incl 1 31)
         (Int.gen_incl 1 31)
         (Int.gen_incl (-2047) 2047)
         (Int.gen_incl (-2047) 2047))
      ~f:(fun (rd, rs1, rs1_initial, offset) ->
        let pc, registers =
          M.test_and_registers
            ~instructions:
              [ op_imm ~funct3:Funct3.Op.Add_or_sub ~rs1:0 ~rd:rs1 ~immediate:rs1_initial
              ; jalr ~rd ~rs1 ~offset
              ]
            sim
        in
        let result = pc land 0xFFFFFFFF in
        let expectation = (rs1_initial + offset) land 0xFFFFFFFE in
        if result <> expectation
        then
          raise_s
            [%message
              "Failed"
                (result : int)
                (expectation : int)
                (rd : int)
                (rs1 : int)
                (registers : int list)]);
    [%expect {| |}]
  ;;

  let%expect_test "sb/lb" =
    let sim = create_sim "sb_lb" in
    let open Quickcheck.Generator in
    Core.protect
      ~finally:(fun () -> M.finalize_sim sim)
      ~f:(fun () ->
        Quickcheck.test
          ~trials:25
          (tuple6
             (Int.gen_incl 1 31)
             (Int.gen_incl 1 31)
             (Int.gen_incl 1 31)
             (Int.gen_incl 64 74)
             (Int.gen_incl (-2047) 2047)
             (Int.gen_incl (-30) 30))
          ~f:(fun (rd, rs1, rs2, rs1_initial, rs2_initial, offset) ->
            if rs1 <> rs2
            then (
              let _pc, registers =
                M.test_and_registers
                  ~instructions:
                    [ op_imm
                        ~funct3:Funct3.Op.Add_or_sub
                        ~rs1:0
                        ~rd:rs1
                        ~immediate:rs1_initial
                    ; op_imm
                        ~funct3:Funct3.Op.Add_or_sub
                        ~rs1:0
                        ~rd:rs2
                        ~immediate:rs2_initial
                    ; store ~funct3:Funct3.Store.Sb ~rs1 ~rs2 ~immediate:offset
                    ; load ~funct3:Funct3.Load.Lbu ~rd ~rs1 ~immediate:offset
                    ]
                  sim
              in
              let result = List.nth_exn registers rd in
              if result <> rs2_initial land 0xFF
              then (
                M.print_ram sim;
                raise_s
                  [%message
                    "Failed"
                      (result : int)
                      ~expectation:(rs2_initial land 0xFF : int)
                      (rd : int)
                      (rs1 : int)
                      (rs2 : int)
                      (rs1_initial : int)
                      (rs2_initial : int)
                      (offset : int)
                      (registers : int list)]))
            else ()));
    [%expect {| |}]
  ;;

  let%expect_test "sh/lh" =
    let sim = create_sim "sh_lh" in
    let open Quickcheck.Generator in
    Quickcheck.test
      ~trials:25
      (tuple6
         (Int.gen_incl 1 31)
         (Int.gen_incl 1 31)
         (Int.gen_incl 1 31)
         (Int.gen_incl 64 74)
         (Int.gen_incl (-2047) 2047)
         (Int.gen_incl (-30) 30))
      ~f:(fun (rd, rs1, rs2, rs1_initial, rs2_initial, offset) ->
        if rs1 <> rs2
        then (
          let rs1_initial = rs1_initial land lnot 1 in
          let offset = offset land lnot 1 in
          let _pc, registers =
            M.test_and_registers
              ~instructions:
                [ op_imm
                    ~funct3:Funct3.Op.Add_or_sub
                    ~rs1:0
                    ~rd:rs1
                    ~immediate:rs1_initial
                ; op_imm
                    ~funct3:Funct3.Op.Add_or_sub
                    ~rs1:0
                    ~rd:rs2
                    ~immediate:rs2_initial
                ; store ~funct3:Funct3.Store.Sh ~rs1 ~rs2 ~immediate:offset
                ; load ~funct3:Funct3.Load.Lhu ~rd ~rs1 ~immediate:offset
                ]
              sim
          in
          let result = List.nth_exn registers rd in
          if result <> rs2_initial land 0xFFFF
          then (
            M.print_ram sim;
            raise_s
              [%message
                "Failed"
                  (result : int)
                  ~expectation:(rs2_initial land 0xFFFF : int)
                  (rd : int)
                  (rs1 : int)
                  (rs2 : int)
                  (rs1_initial : int)
                  (rs2_initial : int)
                  (offset : int)
                  (registers : int list)]))
        else ());
    [%expect {| |}]
  ;;

  let%expect_test "sw/lw" =
    let sim = create_sim "sw_lw" in
    let open Quickcheck.Generator in
    Core.protect
      ~finally:(fun () -> M.finalize_sim sim)
      ~f:(fun () ->
        Quickcheck.test
          ~trials:25
          (tuple6
             (Int.gen_incl 1 31)
             (Int.gen_incl 1 31)
             (Int.gen_incl 1 31)
             (Int.gen_incl 64 74)
             (Int.gen_incl (-2047) 2047)
             (Int.gen_incl (-30) 30))
          ~f:(fun (rd, rs1, rs2, rs1_initial, rs2_initial, offset) ->
            if rs1 <> rs2
            then (
              let rs1_initial = rs1_initial land lnot 0b11 in
              let offset = offset land lnot 0b11 in
              let _pc, registers =
                M.test_and_registers
                  ~instructions:
                    [ op_imm
                        ~funct3:Funct3.Op.Add_or_sub
                        ~rs1:0
                        ~rd:rs1
                        ~immediate:rs1_initial
                    ; op_imm
                        ~funct3:Funct3.Op.Add_or_sub
                        ~rs1:0
                        ~rd:rs2
                        ~immediate:rs2_initial
                    ; store ~funct3:Funct3.Store.Sw ~rs1 ~rs2 ~immediate:offset
                    ; (* TODO: Add Load tests that are sign extended. *)
                      load ~funct3:Funct3.Load.Lw ~rd ~rs1 ~immediate:offset
                    ]
                  sim
              in
              let result = List.nth_exn registers rd in
              if result <> rs2_initial land 0xFFFFFFFF
              then (
                M.print_ram sim;
                raise_s
                  [%message
                    "Failed"
                      (result : int)
                      ~expectation:(rs2_initial land 0xFFFFFFFF : int)
                      (rd : int)
                      (rs1 : int)
                      (rs2 : int)
                      (rs1_initial : int)
                      (rs2_initial : int)
                      (offset : int)
                      (registers : int list)]))
            else ()));
    [%expect {| |}]
  ;;

  let%expect_test "op_imm" =
    let sim = create_sim "test_op_imm" in
    test
      ~instructions:[ op_imm ~funct3:Funct3.Op.Add_or_sub ~rs1:0 ~rd:1 ~immediate:550 ]
      sim;
    [%expect
      {|
      ("PC: " 4 REG:
       (0 550 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
      ("00000000  93 00 60 22 00 00 00 00  00 00 00 00 00 00 00 00  |..`\"............|"
       "00000010  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
       "00000020  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
       "00000030  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
       "00000040  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
       "00000050  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
       "00000060  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
       "00000070  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|")
      |}];
    test
      ~instructions:
        [ op_imm ~funct3:Funct3.Op.Xor ~rs1:0 ~rd:1 ~immediate:0b0101
        ; op_imm ~funct3:Funct3.Op.Xor ~rs1:1 ~rd:1 ~immediate:0b1010
        ]
      sim;
    [%expect
      {|
      ("PC: " 8 REG:
       (0 15 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
      ("00000000  93 40 50 00 93 c0 a0 00  00 00 00 00 00 00 00 00  |.@P.............|"
       "00000010  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
       "00000020  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
       "00000030  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
       "00000040  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
       "00000050  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
       "00000060  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
       "00000070  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|")
      |}];
    test
      ~instructions:
        [ op_imm ~funct3:Funct3.Op.Add_or_sub ~rs1:0 ~rd:1 ~immediate:0b1111
        ; op_imm ~funct3:Funct3.Op.And ~rs1:1 ~rd:1 ~immediate:0b11
        ]
      sim;
    [%expect
      {|
      ("PC: " 8 REG:
       (0 3 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
      ("00000000  93 00 f0 00 93 f0 30 00  00 00 00 00 00 00 00 00  |......0.........|"
       "00000010  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
       "00000020  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
       "00000030  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
       "00000040  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
       "00000050  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
       "00000060  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
       "00000070  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|")
      |}];
    test
      ~instructions:
        [ op_imm ~funct3:Funct3.Op.Add_or_sub ~rs1:0 ~rd:1 ~immediate:0b101010
        ; op_imm ~funct3:Funct3.Op.Or ~rs1:1 ~rd:1 ~immediate:0b010101
        ]
      sim;
    [%expect
      {|
      ("PC: " 8 REG:
       (0 63 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
      ("00000000  93 00 a0 02 93 e0 50 01  00 00 00 00 00 00 00 00  |......P.........|"
       "00000010  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
       "00000020  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
       "00000030  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
       "00000040  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
       "00000050  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
       "00000060  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
       "00000070  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|")
      |}];
    test
      ~instructions:
        [ op_imm ~funct3:Funct3.Op.Add_or_sub ~rs1:0 ~rd:1 ~immediate:0b1
        ; op_imm ~funct3:Funct3.Op.Sll ~rs1:1 ~rd:1 ~immediate:4
        ]
      sim;
    [%expect
      {|
      ("PC: " 8 REG:
       (0 16 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
      ("00000000  93 00 10 00 93 90 40 00  00 00 00 00 00 00 00 00  |......@.........|"
       "00000010  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
       "00000020  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
       "00000030  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
       "00000040  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
       "00000050  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
       "00000060  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
       "00000070  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|")
      |}];
    (* TODO: Test the negative cases *)
    test
      ~instructions:
        [ op_imm ~funct3:Funct3.Op.Add_or_sub ~rs1:0 ~rd:1 ~immediate:10
        ; op_imm ~funct3:Funct3.Op.Slt ~rs1:1 ~rd:2 ~immediate:5
        ; op_imm ~funct3:Funct3.Op.Slt ~rs1:1 ~rd:3 ~immediate:15
        ]
      sim;
    [%expect
      {|
      ("PC: " 12 REG:
       (0 10 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
      ("00000000  93 00 a0 00 13 a1 50 00  93 a1 f0 00 00 00 00 00  |......P.........|"
       "00000010  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
       "00000020  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
       "00000030  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
       "00000040  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
       "00000050  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
       "00000060  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
       "00000070  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|")
      |}];
    test
      ~instructions:
        [ op_imm ~funct3:Funct3.Op.Add_or_sub ~rs1:0 ~rd:1 ~immediate:10
        ; op_imm ~funct3:Funct3.Op.Sltu ~rs1:1 ~rd:2 ~immediate:5
        ; op_imm ~funct3:Funct3.Op.Sltu ~rs1:1 ~rd:3 ~immediate:15
        ]
      sim;
    [%expect
      {|
      ("PC: " 12 REG:
       (0 10 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
      ("00000000  93 00 a0 00 13 b1 50 00  93 b1 f0 00 00 00 00 00  |......P.........|"
       "00000010  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
       "00000020  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
       "00000030  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
       "00000040  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
       "00000050  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
       "00000060  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
       "00000070  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|")
      |}];
    test
      ~instructions:
        [ op_imm ~funct3:Funct3.Op.Add_or_sub ~rs1:0 ~rd:1 ~immediate:16
        ; op_imm ~funct3:Funct3.Op.Srl_or_sra ~rs1:1 ~rd:2 ~immediate:4
        ; op_imm ~funct3:Funct3.Op.Srl_or_sra ~rs1:1 ~rd:3 ~immediate:3
        ]
      sim;
    [%expect
      {|
      ("PC: " 12 REG:
       (0 16 1 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
      ("00000000  93 00 00 01 13 d1 40 00  93 d1 30 00 00 00 00 00  |......@...0.....|"
       "00000010  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
       "00000020  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
       "00000030  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
       "00000040  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
       "00000050  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
       "00000060  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
       "00000070  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|")
      |}];
    (* Test x0 is hardwired to zero. *)
    test
      ~instructions:[ op_imm ~funct3:Funct3.Op.Add_or_sub ~rd:0 ~rs1:0 ~immediate:0x500 ]
      sim;
    [%expect
      {|
      ("PC: " 12 REG:
       (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
      ("00000000  13 00 00 50 13 d1 40 00  93 d1 30 00 00 00 00 00  |...P..@...0.....|"
       "00000010  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
       "00000020  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
       "00000030  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
       "00000040  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
       "00000050  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
       "00000060  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
       "00000070  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|")
      |}];
    M.finalize_sim sim;
    [%expect {| |}]
  ;;

  let%expect_test "op_imm" =
    let sim = create_sim "op" in
    test
      ~instructions:
        [ op_imm ~funct3:Funct3.Op.Add_or_sub ~rs1:0 ~rd:1 ~immediate:500
        ; op_imm ~funct3:Funct3.Op.Add_or_sub ~rs1:0 ~rd:2 ~immediate:300
        ; op ~funct3:Funct3.Op.Add_or_sub ~rs1:1 ~rs2:2 ~rd:4 ~funct7:0
        ]
      sim;
    [%expect
      {|
      ("PC: " 12 REG:
       (0 500 300 0 800 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
      ("00000000  93 00 40 1f 13 01 c0 12  33 82 20 00 00 00 00 00  |..@.....3. .....|"
       "00000010  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
       "00000020  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
       "00000030  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
       "00000040  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
       "00000050  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
       "00000060  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
       "00000070  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|")
      |}];
    M.finalize_sim sim;
    [%expect {| |}]
  ;;

  let%expect_test "branch" =
    let sim = create_sim "branch" in
    test
      ~instructions:
        [ branch ~funct3:Funct3.Branch.Bne ~rs1:0 ~rs2:0 ~offset:200
        ; branch ~funct3:Funct3.Branch.Beq ~rs1:0 ~rs2:0 ~offset:500
        ]
      sim;
    [%expect
      {|
      ("PC: " 504 REG:
       (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
      ("00000000  63 14 00 0c 63 0a 00 1e  00 00 00 00 00 00 00 00  |c...c...........|"
       "00000010  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
       "00000020  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
       "00000030  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
       "00000040  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
       "00000050  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
       "00000060  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
       "00000070  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|")
      |}];
    test
      ~instructions:
        [ op_imm ~funct3:Funct3.Op.Add_or_sub ~rs1:0 ~rd:1 ~immediate:550
        ; branch ~funct3:Funct3.Branch.Beq ~rs1:0 ~rs2:1 ~offset:500
        ]
      sim;
    [%expect
      {|
      ("PC: " 8 REG:
       (0 550 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
      ("00000000  93 00 60 22 63 0a 10 1e  00 00 00 00 00 00 00 00  |..`\"c...........|"
       "00000010  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
       "00000020  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
       "00000030  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
       "00000040  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
       "00000050  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
       "00000060  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
       "00000070  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|")
      |}];
    test
      ~instructions:
        [ op_imm ~funct3:Funct3.Op.Add_or_sub ~rs1:0 ~rd:1 ~immediate:550
        ; branch ~funct3:Funct3.Branch.Bne ~rs1:0 ~rs2:1 ~offset:500
        ]
      sim;
    [%expect
      {|
      ("PC: " 504 REG:
       (0 550 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
      ("00000000  93 00 60 22 63 1a 10 1e  00 00 00 00 00 00 00 00  |..`\"c...........|"
       "00000010  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
       "00000020  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
       "00000030  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
       "00000040  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
       "00000050  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
       "00000060  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
       "00000070  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|")
      |}];
    (* Expect the second branch to be taken *)
    test
      ~instructions:
        [ branch ~funct3:Funct3.Branch.Blt ~rs1:0 ~rs2:1 ~offset:50
        ; op_imm ~funct3:Funct3.Op.Add_or_sub ~rs1:0 ~rd:1 ~immediate:100
        ; branch ~funct3:Funct3.Branch.Blt ~rs1:0 ~rs2:1 ~offset:250
        ]
      sim;
    [%expect
      {|
      ("PC: " 258 REG:
       (0 100 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
      ("00000000  63 49 10 02 93 00 40 06  63 4d 10 0e 00 00 00 00  |cI....@.cM......|"
       "00000010  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
       "00000020  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
       "00000030  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
       "00000040  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
       "00000050  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
       "00000060  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
       "00000070  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|")
      |}];
    (* Expect the second branch to be taken *)
    test
      ~instructions:
        [ op_imm ~funct3:Funct3.Op.Add_or_sub ~rs1:0 ~rd:1 ~immediate:100
        ; branch ~funct3:Funct3.Branch.Bge ~rs1:0 ~rs2:1 ~offset:50
        ; branch ~funct3:Funct3.Branch.Bge ~rs1:1 ~rs2:0 ~offset:250
        ]
      sim;
    [%expect
      {|
      ("PC: " 258 REG:
       (0 100 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
      ("00000000  93 00 40 06 63 59 10 02  63 dd 00 0e 00 00 00 00  |..@.cY..c.......|"
       "00000010  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
       "00000020  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
       "00000030  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
       "00000040  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
       "00000050  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
       "00000060  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
       "00000070  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|")
      |}];
    (* TODO: Test unsigned variants and negative numbers. *)
    M.finalize_sim sim;
    [%expect {| |}]
  ;;
end

module Cpu_with_no_io_controller =
  Cpu.Make
    (struct
      let register_width = Register_width.B32
      let num_registers = 32
    end)
    (struct
      let num_bytes = 128
    end)
    (struct
      let num_harts = 1
      let include_io_controller = Io_controller_config.No_io_controller
    end)

module With_manually_programmed_ram = Make (struct
    type sim =
      ( Bits.t ref Cpu_with_no_io_controller.I.t
        , Bits.t ref Cpu_with_no_io_controller.O.t )
        Cyclesim.t
      * Waveform.t
      * string

    let create_sim name : sim =
      let module Sim =
        Cyclesim.With_interface
          (Cpu_with_no_io_controller.I)
          (Cpu_with_no_io_controller.O)
      in
      let sim =
        Sim.create
          ~config:Cyclesim.Config.trace_all
          (Cpu_with_no_io_controller.create
             (Scope.create ~auto_label_hierarchical_ports:true ~flatten_design:true ()))
      in
      let waveform, sim = Waveform.create sim in
      sim, waveform, name
    ;;

    let finalize_sim sim =
      if debug
      then (
        let _, waveform, name = sim in
        Waveform.Serialize.marshall waveform ("/tmp/" ^ name))
      else ()
    ;;

    let clear_registers ~(inputs : Bits.t ref Cpu_with_no_io_controller.I.t) sim =
      inputs.clear := Bits.vdd;
      Cyclesim.cycle sim;
      Cyclesim.cycle sim;
      inputs.clear := Bits.gnd
    ;;

    let test_and_registers ~instructions sim =
      let sim, _, _ = sim in
      (* Initialize the main memory to some known values for testing. *)
      Test_util.program_ram sim (Array.of_list instructions);
      let inputs : _ Cpu_with_no_io_controller.I.t = Cyclesim.inputs sim in
      let _outputs_before : _ Cpu_with_no_io_controller.O.t =
        Cyclesim.outputs ~clock_edge:Side.Before sim
      in
      let outputs : _ Cpu_with_no_io_controller.O.t = Cyclesim.outputs sim in
      clear_registers ~inputs sim;
      let rec loop_for cycles =
        if cycles = 0
        then ()
        else (
          Cyclesim.cycle sim;
          loop_for (cycles - 1))
      in
      loop_for 50;
      match outputs.registers with
      | [ outputs ] ->
        let outputs =
          Cpu_with_no_io_controller.Registers.map ~f:(fun t -> Bits.to_int !t) outputs
        in
        outputs.pc, outputs.general
      | _ -> raise_s [%message "BUG: Unexpected number of harts"]
    ;;

    let print_ram (sim, _, _) = Test_util.print_ram sim

    let test ~instructions sim =
      let pc, registers = test_and_registers ~instructions sim in
      print_s [%message "PC: " ~_:(pc : int) "REG:" ~_:(registers : int list)];
      print_ram sim
    ;;
  end)

let uart_config =
  { Hardcaml_uart_controller.Config.clock_frequency = 200
  ; baud_rate = 200
  ; include_parity_bit = true
  ; stop_bits = 1
  }
;;

module Uart_tx = Uart_tx.Make (struct
    let config = uart_config
  end)

module Cpu_with_dma_memory =
  Cpu.Make
    (struct
      let register_width = Register_width.B32
      let num_registers = 32
    end)
    (struct
      let num_bytes = 128
    end)
    (struct
      let num_harts = 1
      let include_io_controller = Io_controller_config.Uart_controller uart_config
    end)

module With_transmitter = struct
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
    type 'a t = { registers : 'a Cpu_with_dma_memory.Registers.t list [@length 1] }
    [@@deriving hardcaml]
  end

  let create scope { I.clock; clear; data_in_valid; data_in } =
    let { Uart_tx.O.uart_tx; _ } =
      Uart_tx.hierarchical
        ~instance:"tx"
        scope
        { Uart_tx.I.clock; clear; data_in_valid; data_in }
    in
    let { Cpu_with_dma_memory.O.registers; _ } =
      Cpu_with_dma_memory.hierarchical
        ~instance:"cpu"
        scope
        { clock; clear; uart_rx = uart_tx }
    in
    { O.registers }
  ;;
end

module With_dma_ram = Make (struct
    type sim =
      (Bits.t ref With_transmitter.I.t, Bits.t ref With_transmitter.O.t) Cyclesim.t
      * Waveform.t
      * string

    let create_sim name : sim =
      let module Sim = Cyclesim.With_interface (With_transmitter.I) (With_transmitter.O)
      in
      let sim =
        Sim.create
          ~config:Cyclesim.Config.trace_all
          (With_transmitter.create
             (Scope.create ~auto_label_hierarchical_ports:true ~flatten_design:true ()))
      in
      let waveform, sim = Waveform.create sim in
      sim, waveform, name
    ;;

    let finalize_sim sim =
      if debug
      then (
        let _, waveform, name = sim in
        Waveform.Serialize.marshall waveform ("/tmp/dma_" ^ name))
      else ()
    ;;

    let print_ram (sim, _, _) = Test_util.print_ram sim

    let clear_registers ~(inputs : Bits.t ref With_transmitter.I.t) sim =
      inputs.clear := Bits.vdd;
      Cyclesim.cycle sim;
      inputs.clear := Bits.gnd
    ;;

    let send_bits sim whole_packet =
      (* Send the message through byte by byte. Uart_tx will transmit a
         * byte once every ~10 cycles (this is dependent on the number of stop
         * bits and the parity bit. *)
      let inputs : _ With_transmitter.I.t = Cyclesim.inputs sim in
      let rec loop_for n =
        if n = 0
        then ()
        else (
          Cyclesim.cycle sim;
          loop_for (n - 1))
      in
      List.iter
        ~f:(fun input ->
          inputs.data_in_valid := vdd;
          inputs.data_in := of_int ~width:8 input;
          Cyclesim.cycle sim;
          inputs.data_in_valid := of_int ~width:1 0;
          (* TODO: Tighter loop *)
          loop_for 20)
        whole_packet
    ;;

    let send_dma_message ~address ~packet sim =
      (* TODO: Move this to a util section *)
      let whole_packet = dma_packet ~address packet in
      send_bits sim whole_packet
    ;;

    let test_and_registers ~instructions sim =
      let sim, _, _ = sim in
      let inputs : _ With_transmitter.I.t = Cyclesim.inputs sim in
      (* Send a clear signal to initialize any CPU IO controller state back to
       * default so we're ready to receive. *)
      clear_registers ~inputs sim;
      send_dma_message
        ~address:0
        ~packet:
          (Bits.concat_lsb instructions
           |> Bits.split_lsb ~part_width:8
           |> List.map ~f:Bits.to_char
           |> String.of_char_list)
        sim;
      send_bits sim clear_packet;
      (* We need to wait for 256 cycles because that is how long the design
         holds the clear signal for. *)
      Sequence.range 0 256 |> Sequence.iter ~f:(fun _ -> Cyclesim.cycle sim);
      let _outputs_before : _ With_transmitter.O.t =
        Cyclesim.outputs ~clock_edge:Side.Before sim
      in
      let outputs : _ With_transmitter.O.t = Cyclesim.outputs sim in
      let rec loop_for cycles =
        if cycles = 0
        then ()
        else (
          Cyclesim.cycle sim;
          loop_for (cycles - 1))
      in
      loop_for 50;
      match outputs.registers with
      | [ outputs ] ->
        let outputs =
          Cpu_with_dma_memory.Registers.map ~f:(fun t -> Bits.to_int !t) outputs
        in
        outputs.pc, outputs.general
      | _ -> raise_s [%message "BUG: Unexpected number of harts"]
    ;;

    let test ~instructions sim =
      let pc, registers = test_and_registers ~instructions sim in
      print_s [%message "PC: " ~_:(pc : int) "REG:" ~_:(registers : int list)];
      print_ram sim
    ;;
  end)
(* TODO: Add discrete SH and SB quickcheck tests along with LH and LW since they are paired it is easy to break them both at the same time. *)
