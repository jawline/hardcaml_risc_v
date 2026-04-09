open! Core
open Program_cpu_shared
open Tsdl

module Event_ring_state = struct
  type t =
    { base_address : int
    ; mutable next_ring_event : int
    ; ring_size : int
    ; writer : Out_channel.t
    }

  let encode key =
    match key with
    (* I worked these out by looking at doomkeys.h in the doomgeneric source code. *)
    | "W" -> Some 0xad
    | "A" -> Some 0xac
    | "D" -> Some 0xae
    | "S" -> Some 0xaf
    | "Q" -> Some 0xa0
    | "E" -> Some 0xa1
    | "F" -> Some 0xa2
    | "Escape" -> Some 27
    | "Tab" -> Some 9
    | "Return" -> Some 13
    | "PageUp" -> Some 0xC9
    | "PageDown" -> Some 0xD1
    | "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" ->
      Char.to_int (String.get key 0) |> Some
    | "B"
    | "C"
    | "G"
    | "H"
    | "I"
    | "J"
    | "K"
    | "L"
    | "M"
    | "N"
    | "O"
    | "P"
    | "R"
    | "T"
    | "U"
    | "V"
    | "X"
    | "Y"
    | "Z" -> String.get key 0 |> Char.lowercase |> Char.to_int |> Some
    | "Left Ctrl" | "Right Ctrl" -> Some 0xa3 (* Shooty shooty *)
    | _ ->
      print_s [%message "Unmapped key" (key : string)];
      None
  ;;

  let send ~key ~down t =
    match encode key with
    | Some key_encoded ->
      let key_press_bytes =
        [ 0b1000_0000; 0b0000_0000; Bool.to_int down; key_encoded ]
        |> List.rev
        (* Little endian *) |> List.map ~f:Char.of_int_exn
        |> String.of_char_list
      in
      let address = t.base_address + (t.next_ring_event * 4) in
      print_s
        [%message
          "Sending key"
            (t.next_ring_event : int)
            (address : Int.Hex.t)
            (key_encoded : Int.Hex.t)
            (down : bool)];
      send_chunk ~writer:t.writer ~address ~chunk:key_press_bytes;
      t.next_ring_event <- (t.next_ring_event + 1) % t.ring_size
    | None -> ()
  ;;
end

let tsdl_window ~ring () =
  match Sdl.init Sdl.Init.video with
  | Error (`Msg e) ->
    Sdl.log "Init error: %s" e;
    exit 1
  | Ok () ->
    print_s [%message "Here"];
    (* Create a hidden window to capture keyboard focus *)
    (match Sdl.create_window "Key DMA Capture" ~w:800 ~h:600 Sdl.Window.shown with
     | Error (`Msg e) ->
       Sdl.log "Window error: %s" e;
       exit 1
     | Ok w ->
       let event = Sdl.Event.create () in
       let rec loop () =
         if Sdl.poll_event (Some event)
         then (
           match Sdl.Event.(get event typ) with
           | t when t = Sdl.Event.key_down ->
             let key = Sdl.Event.(get event keyboard_keycode) in
             Printf.printf "Key Down: %s\n%!" (Sdl.get_key_name key);
             Event_ring_state.send ~key:(Sdl.get_key_name key) ~down:true ring;
             loop ()
           | t when t = Sdl.Event.key_up ->
             let key = Sdl.Event.(get event keyboard_keycode) in
             Printf.printf "Key Up: %s\n%!" (Sdl.get_key_name key);
             Event_ring_state.send ~key:(Sdl.get_key_name key) ~down:false ring;
             loop ()
           | t when t = Sdl.Event.quit -> ()
           | _ -> loop ())
         else (
           Sdl.delay 10l;
           (* Prevent high CPU usage *)
           loop ())
       in
       loop ();
       Sdl.destroy_window w;
       Sdl.quit ())
;;

let command =
  Command.basic
    ~summary:"program running design and then listen for output"
    (let open Command.Let_syntax in
     let open Command.Param in
     let%map device_filename = anon ("device-filename" %: string) in
     fun () ->
       let settings = Uart_settings.default ~clock_frequency:0 in
       let writer, _reader =
         open_with_stty_settings
           ~baud_rate:settings.baud_rate
           ~stop_bits:settings.stop_bits
           ~parity_bit:settings.include_parity_bit
           ~device_filename
       in
       let ring =
         { Event_ring_state.base_address = 134216704
         ; next_ring_event = 0
         ; ring_size = 128
         ; writer
         }
       in
       print_s [%message "Starting TSDL"];
       tsdl_window ~ring ())
;;

let () = Command_unix.run command
