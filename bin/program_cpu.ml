open! Core
open Hardcaml_risc_v_test
open Opcode_helper

let read_byte ~in_channel () =
  let byte = In_channel.input_byte in_channel |> Option.value_exn in
  printf "%02x " (byte);
  byte
;;

let read_packet t =
  let read_byte = read_byte ~in_channel:t in
  let rec wait_for_header () =
    let header = read_byte () |> Char.of_int_exn in
    print_s [%message "RD" (header : char)];
    if Char.(header <> 'D') then wait_for_header () else header
  in
  let header = wait_for_header () in
  let length_msb = read_byte () in
  let length_lsb = read_byte () in
  let length = (length_msb lsl 8) lor length_lsb in
  print_s [%message (length : int) (length_msb : int) (length_lsb : int)];
  let bytes_ =
    List.init ~f:(fun _ -> read_byte () |> Char.of_int_exn) length
    |> List.rev
    |> String.of_char_list
  in
  header, length, bytes_
;;

let do_write ~ch data =
  List.iter ~f:(fun byte -> Out_channel.output_byte ch byte) data;
  Out_channel.flush ch;
  ()
;;

(** Open a serial device, configure the baud rate, stop bits and parity bit,
    then convert the file descriptor into input and output channels and return
    them. *)
let open_with_stty_settings ~baud_rate ~stop_bits ~parity_bit ~device_filename =
  let file_descr = Core_unix.openfile ~mode:[ O_RDWR ] device_filename in
  let tio_attr = Core_unix.Terminal_io.tcgetattr file_descr in
  let new_attrs : Core_unix.Terminal_io.t =
    { c_brkint = false
    ; c_istrip = false
    ; c_obaud = baud_rate
    ; c_ibaud = baud_rate
    ; c_csize = 8
    ; c_cstopb = stop_bits
    ; c_parenb = parity_bit
    ; c_icanon = false
    ; c_isig = false
    ; c_echo = false
    ; c_echoe = false
    ; c_echok = false
    ; c_echonl = false
    ; c_noflsh = true
    ; c_veof = tio_attr.c_veof
    ; c_opost = false
    ; c_ignbrk = true
    ; c_ignpar = false
    ; c_parmrk = false
    ; c_inpck = parity_bit
    ; c_inlcr = false
    ; c_igncr = true
    ; c_icrnl = false
    ; c_ixon = false
    ; c_ixoff = false
    ; c_cread = true
    ; c_parodd = false
    ; c_hupcl = false
    ; c_clocal = false
    ; c_vintr = tio_attr.c_vintr
    ; c_vquit = tio_attr.c_vquit
    ; c_verase = tio_attr.c_verase
    ; c_vkill = tio_attr.c_vkill
    ; c_veol = tio_attr.c_veol
    ; c_vmin = tio_attr.c_vmin
    ; c_vtime = tio_attr.c_vtime
    ; c_vstart = tio_attr.c_vstart
    ; c_vstop = tio_attr.c_vstop
    }
  in
  Core_unix.Terminal_io.tcsetattr new_attrs file_descr ~mode:TCSANOW;
  Core_unix.out_channel_of_descr file_descr, Core_unix.in_channel_of_descr file_descr
;;

(** Program a device with a chunk of memory at a specific address. The address
    must be register width aligned. *)
let send_chunk ~writer ~address ~chunk =
  let formatted_packet = dma_packet ~address chunk in
  do_write ~ch:writer formatted_packet
;;

(** Read packets from the serial device and print them out. *)
let rec print_any_incoming_packets ~reader =
  let header, length, bytes_ = read_packet reader in
  printf "%c %i %s\n%!" header length bytes_;
  print_any_incoming_packets ~reader
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
       print_any_incoming_packets ~reader)
;;

let () = Command_unix.run command
