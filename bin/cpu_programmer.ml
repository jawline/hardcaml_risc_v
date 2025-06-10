open! Core
open Hardcaml_risc_v_test
open Opcode_helper

let read_packet t =
  let rec wait_for_header () =
    let header = In_channel.input_char t |> Option.value_exn in
    print_s [%message "RD" (header : char)];
    if Char.(header <> 'D') then wait_for_header () else header
  in
  let header = wait_for_header () in
  let length_msb = In_channel.input_byte t |> Option.value_exn in
  let length_lsb = In_channel.input_byte t |> Option.value_exn in
  let length = (length_msb lsl 8) lor length_lsb in
  let bytes_ =
    List.init ~f:(fun _ -> In_channel.input_char t |> Option.value_exn) length
    |> List.rev
    |> String.of_char_list
  in
  header, length, bytes_
;;

let device_file = "/dev/ttyUSB1"

let do_write ~ch data =
  List.iter ~f:(fun byte -> Out_channel.output_byte ch byte) data;
  Out_channel.flush ch;
  let _ = Core_unix.nanosleep 1. in
  ()
;;

let command =
  Command.basic
    ~summary:"program running design and then listen for output"
    (let open Command.Let_syntax in
     let open Command.Param in
     let%map program_filename = anon ("program-filename" %: string) in
     fun () ->
       printf "Opening out channel\n";
       let writer = Out_channel.create ~binary:true device_file in
       printf "Opening in channel\n%!";
       let reader = In_channel.create ~binary:true device_file in
       print_s [%message "Loading" (program_filename : string)];
       let program = In_channel.read_all program_filename in
       print_s [%message "Loaded" (String.length program : int)];
       let chunk_sz = 320000 in
       String.to_list program
       |> List.chunks_of ~length:chunk_sz
       |> List.iteri ~f:(fun index chunk ->
         print_s [%message "Sending chunk"];
         let address = index * chunk_sz in
         let program = String.of_char_list chunk in
         let formatted_packet = dma_packet ~address program in
         do_write ~ch:writer formatted_packet);
       printf "Sending clear signal via DMA\n%!";
       do_write ~ch:writer clear_packet;
       printf "Waiting\n%!";
       let rec loop () =
         let header, length, bytes_ = read_packet reader in
         printf "%c %i %s\n%!" header length bytes_;
         loop ()
       in
       loop ())
;;

let () = Command_unix.run command
