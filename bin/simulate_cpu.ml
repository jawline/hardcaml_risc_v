open! Core
open Core_unix
open Terminal_io
open Hardcaml_risc_v_regression_tests
open Test_base

let set_raw_mode fd =
  let mode = tcgetattr fd in
  let raw_mode =
    { mode with
      c_icanon = false
    ; c_echo = false
    ; c_isig = true
    ; c_ixon = false
    ; c_icrnl = false
    ; c_opost = true
    ; c_vmin = 1
    ; c_vtime = 0
    }
  in
  tcsetattr raw_mode fd ~mode:TCSAFLUSH;
  mode
;;

let restore_mode fd original_mode = tcsetattr original_mode fd ~mode:TCSAFLUSH

let with_raw_mode ~f =
  let fd = Core_unix.descr_of_out_channel Out_channel.stdout in
  if not (isatty fd)
  then raise_s [%message "Cannot enter raw"]
  else (
    let original_mode = set_raw_mode fd in
    try
      f ();
      restore_mode fd original_mode
    with
    | e ->
      restore_mode fd original_mode;
      raise e)
;;

let clear_screen () = Out_channel.output_string Out_channel.stdout "\x1b[2J"
let move_cursor_home () = Out_channel.output_string Out_channel.stdout "\x1b[H"

let hide_cursor () =
  Out_channel.output_string Out_channel.stdout "\x1b[?25l";
  Out_channel.flush Out_channel.stdout
;;

let without_raw_mode ~f = f ()

let sim =
  Command.basic
    ~summary:"Simulate"
    (let open Command.Let_syntax in
     let open Command.Param in
     let%map path = flag "path" (required string) ~doc:"path to executable"
     and raw_mode = flag "raw-mode" (optional bool) ~doc:"use raw mode or not"
     and print_frames =
       flag "print-frames" (optional bool) ~doc:"print the framebuffer to stdout"
     and run_for_cycles =
       flag "run-for-cycles" (optional int) ~doc:"num cycles to run for"
     in
     fun () ->
       let raw_mode = Option.value ~default:true raw_mode in
       let run_for_cycles = Option.value ~default:100_000_000_000_000 run_for_cycles in
       let tui_fn = if raw_mode then with_raw_mode else without_raw_mode in
       tui_fn ~f:(fun () ->
         print_s [%message "Starting up"];
         let program = In_channel.read_all path in
         print_s [%message "Loaded program"];
         let sim = Test_base.base_sim ~trace:false in
         if raw_mode then hide_cursor ();
         test
           ~directly_program_ram:true
           ~before_printing_frame:(fun () ->
             if raw_mode
             then (
               clear_screen ();
               move_cursor_home ()))
           ~print_frames:(Option.value ~default:true print_frames)
           ~cycles:run_for_cycles
           ~data:program
           sim))
;;

let all_commands = Command.group ~summary:"sim tools" [ "sim", sim ]
let () = Command_unix.run all_commands
