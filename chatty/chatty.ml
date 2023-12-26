open Riot
open Minttea
open Leaves

type message = {
  channel : string;
  body : [ `System of string | `Twitch of Twitch.message ];
}

type model = {
  user : string;
  channel : string;
  input_field : Text_input.t;
  quitting : bool;
  channels : (string, message list) Hashtbl.t;
}

let init _ =
  Twitch.add_handler (self ());
  Twitch.validate_requirements ();
  Twitch.authenticate ~nick:"leostera" ();
  Twitch.join [ "leostera" ];
  Command.Noop

let add_message (msg : message) model =
  let messages =
    match Hashtbl.find_opt model.channels msg.channel with
    | None -> [ msg ]
    | Some msgs -> msg :: msgs
  in
  Hashtbl.replace model.channels msg.channel messages;
  model

let update (event : Event.t) model =
  match event with
  | Event.Custom (Twitch.Message msg) ->
      let msg = { channel = model.channel; body = `Twitch msg } in
      (add_message msg model, Command.Noop)
  | Event.Custom Twitch.Login_successful ->
      let msg = { channel = model.channel; body = `System "logged in" } in
      (add_message msg model, Command.Noop)
  | Event.KeyDown Escape -> ({ model with quitting = true }, Command.Quit)
  | Event.KeyDown Enter ->
      let msg = Text_input.current_text model.input_field in
      let model =
        if msg = "" then model
        else (
          Twitch.send_message ~channel:model.channel ~msg;
          let msg =
            Twitch.
              { channel = model.channel; user = model.user; msg = [ ":"; msg ] }
          in
          let msg = { channel = model.channel; body = `Twitch msg } in
          add_message msg model)
      in
      let input_field = Text_input.empty () in
      ({ model with input_field }, Command.Noop)
  | _ ->
      let input_field = Text_input.update model.input_field event in
      ({ model with input_field }, Command.Noop)

let view model =
  let messages =
    Hashtbl.find_opt model.channels model.channel |> Option.value ~default:[]
  in
  let messages = List.rev messages in
  let messages =
    messages
    |> List.map (fun msg ->
           match msg.body with
           | `System msg -> Format.sprintf "system < %s" msg
           | `Twitch msg ->
               let user =
                 List.nth (String.split_on_char '!' msg.user) 0
                 |> Stringext.replace_all ~pattern:":" ~with_:""
               in
               let msg = Stringext.drop (String.concat " " msg.msg) 1 in
               Format.sprintf "%s > %s" user msg)
    |> String.concat "\n"
  in
  messages ^ "\n\n> " ^ Text_input.current_text model.input_field

let app = Minttea.app ~init ~update ~view ()

let init () =
  let initial_model =
    {
      user = "leostera";
      channel = "#leostera";
      input_field = Text_input.empty ();
      quitting = false;
      channels = Hashtbl.create 32;
    }
  in
  Minttea.run ~initial_model app

let start () =
  Logger.set_log_level None;
  let pid = spawn_link (fun () -> init ()) in
  Ok pid

let name = "Chatty"
