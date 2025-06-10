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

let do_write ~ch data =
  List.iter ~f:(fun byte -> Out_channel.output_byte ch byte) data;
  Out_channel.flush ch;
  let _ = Core_unix.nanosleep 1. in
  ()
;;

(** Open a serial device, configure the baud rate, stop bits and parity bit,
    then convert the file descriptor into input and output channels and return
    them. *)
let open_with_stty_settings ~baud_rate ~stop_bits ~parity_bit ~device_filename =
  let file_descr = Core_unix.openfile ~mode:[ O_RDWR ] device_filename in
  let tio_attr = Core_unix.Terminal_io.tcgetattr file_descr in
  tio_attr.c_obaud <- baud_rate;
  tio_attr.c_ibaud <- baud_rate;
  tio_attr.c_csize <- 8;
  tio_attr.c_cstopb <- stop_bits;
  tio_attr.c_parenb <- parity_bit;
  Core_unix.out_channel_of_descr file_descr, Core_unix.in_channel_of_descr file_descr
;;

(** Program a device with a chunk of memory at a specific address. The address
    must be register width aligned. *)
let send_chunk ~writer ~address ~chunk =
  let formatted_packet = dma_packet ~address chunk in
  do_write ~ch:writer formatted_packet
;;

(** Read packets from the serial device and print them out. *)
let rec print_any_incoming_packets () =
  let header, length, bytes_ = read_packet reader in
  printf "%c %i %s\n%!" header length bytes_;
  print_any_incoming_packets ()
;;

let command =
  Command.basic
    ~summary:"program running design and then listen for output"
    (let open Command.Let_syntax in
     let open Command.Param in
     let%map device_filename = anon ("device-filename" %: string)
     and program_filename = anon ("program-filename" %: string) in
     fun () ->
       printf "Opening device\n";
       (* Pick an arbitrary clock frequency, it doesn't matter for stty settings. *)
       let settings = Uart_settings.default ~clock_frequency:0 in
       let writer, reader =
         open_with_stty_settings
           ~baud_rate:settings.baud_rate
           ~stop_bits:settings.stop_bits
           ~parity_bit:settings.include_parity_bit
           ~device_filename
       in
       print_s [%message "Loading program" ~_:(program_filename : string)];
       let program = In_channel.read_all program_filename in
       print_s [%message "Progam length: " ~_:(String.length program : int)];
       (* Split our program into 1024 byte chunks and send it. *)
       let chunk_sz = 1024 in
       String.to_list program
       |> List.chunks_of ~length:chunk_sz
       |> List.map ~f:String.of_char_list
       |> List.iteri ~f:(fun index chunk ->
         send_chunk ~writer ~address:(index * chunk_sz) ~chunk);
       (* Send a clear signal to the device. *)
       print_s [%message "Sending clear signal via DMA"];
       do_write ~ch:writer clear_packet;
       print_s [%message "Printing any received packets"];
       loop ())
;;

let () = Command_unix.run command
