open! Core
open Program_cpu_shared

let command =
  Command.basic
    ~summary:"program running design and then listen for output"
    (let open Command.Let_syntax in
     let open Command.Param in
     let%map device_filename = anon ("device-filename" %: string)
     and program_filename = anon ("program-filename" %: string) in
     fun () ->
       let _writer, reader =
         program_cpu ~skip_program:false ~device_filename ~program_filename
       in
       print_s [%message "Printing any received packets"];
       print_any_incoming_packets ~reader)
;;

let () = Command_unix.run command
