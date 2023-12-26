open Riot

let print_error err =
  match err with
  | `Eof -> Logger.error (fun f -> f "end of file wtf")
  | `Bad_uri uri -> Logger.error (fun f -> f "bad_uri: %a" Uri.pp uri)
  | `Closed -> Logger.error (fun f -> f "connection closed")
  | `Must_have_requirements -> Logger.error (fun f -> f "no requirements found")
  | `Parse_error raw ->
      Logger.error (fun f -> f "error parsing command: %s" raw)
  | `Timeout -> Logger.error (fun f -> f "connection timed out")
  | `Unix_error error ->
      Logger.error (fun f -> f "unix error: %s" (Unix.error_message error))
  | `Unmet_requirement reqs ->
      Logger.error (fun f ->
          f "could not meet requirements: %s" (String.concat " " reqs))

type message = { channel : string; user : string; msg : string list }
type Message.t += Message of message | Login_successful

let ( let* ) = Result.bind

module Socket_reader = struct
  type Message.t += Irc_command of Irc.reply

  let rec loop ?(carry_over = "") recv conn buf =
    let* _ = Net.Socket.receive ~buf conn in
    let raw = IO.Buffer.to_string buf in
    Logger.debug (fun f -> f "chatty < %s" raw);
    let lines =
      carry_over ^ raw
      |> Stringext.replace_all ~pattern:"\r\n" ~with_:"\n"
      |> String.split_on_char '\n'
      |> List.filter (fun res -> String.length res > 0)
    in
    let has_carry_over = not (String.ends_with ~suffix:"\r\n" raw) in
    Logger.debug (fun f -> f "has_carry_over? %b" has_carry_over);

    let* cmds, carry_over = Irc.parse has_carry_over lines [] in
    List.iter (fun cmd -> send recv (Irc_command cmd)) cmds;
    loop ~carry_over recv conn buf

  let start_link conn recv =
    let buf = IO.Buffer.with_capacity 1024 in
    let pid =
      spawn_link (fun () ->
          match loop recv conn buf with
          | Ok _ -> ()
          | Error err -> print_error err)
    in
    Ok pid
end

module Connection = struct
  type Message.t +=
    | Send_message of { channel : string; msg : string }
    | Authenticate of { nick : string; pass : string }
    | Validate_requirements of { reqs : Irc.capability list }
    | Join of { channels : string list }
    | Add_handler of Pid.t

  type config = { url : Uri.t }
  type state = { conn : Net.Socket.stream_socket; handlers : Pid.t list }

  let cmd state c =
    Irc.print_command c;
    let str = Irc.cmd_to_string c ^ "\r\n" in
    let data = IO.Buffer.of_string str in
    let writer = Net.Socket.to_writer state.conn in
    let* _len = IO.write_all writer ~data in
    Ok ()

  let rec loop state =
    match receive () with
    | Add_handler pid -> loop { state with handlers = pid :: state.handlers }
    | Send_message { channel; msg } -> handle_send_message ~channel ~msg state
    | Authenticate { nick; pass } -> handle_authenticate ~nick ~pass state
    | Join { channels } -> handle_join ~channels state
    | Validate_requirements { reqs } -> handle_validate_reqs ~reqs state
    | Socket_reader.Irc_command irc_reply -> handle_reply irc_reply state
    | _ -> loop state

  and handle_reply irc_reply state =
    match irc_reply with
    | PING ->
        let* () = cmd state Irc.PONG in
        loop state
    | Irc.PART | Irc.JOINED _ | Irc.LOGIN_SUCCESSFUL | Irc.CAP_ACK _
    | Irc.CAP_NAK _ | Irc.NOTICE _ | Irc.USERSTATE _ | Irc.ROOMSTATE _ ->
        loop state
    | Irc.RECV_PRIVMSG { user; channel; msg; _ } ->
        let msg = Message { channel; user; msg } in
        List.iter
          (fun pid ->
            Logger.debug (fun f -> f "sending message to %a" Pid.pp pid);
            send pid msg)
          state.handlers;
        loop state

  and handle_send_message ~channel ~msg state =
    Logger.debug (fun f -> f "sending message %s" msg);
    let* () = cmd state Irc.(SEND_PRIVMSG { channel; msg }) in
    loop state

  and handle_authenticate ~nick ~pass state =
    let* () = cmd state Irc.(PASS pass) in
    let* () = cmd state Irc.(NICK nick) in
    loop state

  and handle_join ~channels state =
    let* () = cmd state Irc.(JOIN channels) in
    loop state

  and handle_validate_reqs ~reqs state =
    let* () = cmd state Irc.(CAP_REQ reqs) in
    loop state

  let init config =
    let* addr =
      config.url |> Net.Addr.of_uri
      |> Option.to_result ~none:(`Bad_uri config.url)
    in
    let* conn = Net.Socket.connect addr in
    let* _reader = Socket_reader.start_link conn (self ()) in
    let state = { conn; handlers = [] } in
    loop state

  let default_url = "irc://irc.chat.twitch.tv:6667" |> Uri.of_string
  let config ?(url = default_url) () = { url }

  (* NOTE(@leostera): turn this into a supervisor! *)
  let start_link config =
    Logger.error (fun f -> f "Starting Twtich conn\n\n");
    let pid =
      spawn_link (fun () ->
          match init config with Ok _ -> () | Error err -> print_error err)
    in
    Ok pid

end

let name = "Twitch"
let registered_name = "Twitch.main_proc"

let start () =
  let* twitch =
    let config = Connection.config () in
    Connection.start_link config
  in
  register registered_name twitch;
  Ok twitch

let add_handler pid =
  send_by_name ~name:registered_name Connection.(Add_handler pid)

let send_message ~channel ~msg =
  send_by_name ~name:registered_name Connection.(Send_message { channel; msg })

let default_token () = Sys.getenv "CHATTY_TWITCH_TOKEN"

let authenticate ?(pass = default_token ()) ~nick () =
  send_by_name ~name:registered_name Connection.(Authenticate { nick; pass })

let join channels =
  send_by_name ~name:registered_name Connection.(Join { channels })

let default_capabilities = Irc.[ Commands; Membership; Tags ]

let validate_requirements ?(reqs = default_capabilities) () =
  send_by_name ~name:registered_name Connection.(Validate_requirements { reqs })
