open Riot

type capability = Commands | Membership | Tags

let capability_to_string cap =
  let prefix = "twitch.tv/" in
  match cap with
  | Commands -> prefix ^ "commands"
  | Membership -> prefix ^ "membership"
  | Tags -> prefix ^ "tags"

type reply =
  | PART (* :systemfault!systemfault@systemfault.tmi.twitch.tv PART #leostera *)
  | PING  (** PING :tmi.twitch.tv *)
  | JOINED of { channel : string; users : string list }
      (**
       :leostera!leostera@leostera.tmi.twitch.tv JOIN #leostera
       :leostera.tmi.twitch.tv 353 leostera = #leostera :leostera
       :leostera.tmi.twitch.tv 366 leostera #leostera :End of /NAMES list
     *)
  | LOGIN_SUCCESSFUL
      (** 
        :tmi.twitch.tv 001 leostera :Welcome, GLHF!
        :tmi.twitch.tv 002 leostera :Your host is tmi.twitch.tv
        *)
  | CAP_ACK of string list
      (** :tmi.twitch.tv CAP * ACK :twitch.tv/membership twitch.tv/tags *)
  | CAP_NAK of string list
      (** :tmi.twitch.tv CAP * NAK :twitch.tv/commafdiojfodis *)
  | NOTICE of string list
      (** :tmi.twitch.tv NOTICE * :Login authentication failed *)
  | USERSTATE of { meta : string; channel : string }
      (** @badge-info=;badges=moderator/1;color=#FF4500;display-name=mybot;emote-sets=0,300374282;mod=1;subscriber=0;user-type=mod :tmi.twitch.tv USERSTATE #bar *)
  | ROOMSTATE of { meta : string; channel : string }
      (** @badge-info=;badges=moderator/1;color=#FF4500;display-name=mybot;emote-sets=0,300374282;mod=1;subscriber=0;user-type=mod :tmi.twitch.tv ROOMSTATE #bar *)
  | RECV_PRIVMSG of {
      meta : string;
      user : string;
      channel : string;
      msg : string list;
    }
      (** @badge-info=subscriber/60;badges=broadcaster/1,subscriber/0,sub-gifter/5;color=#FF0000;display-name=leostera;emotes=;first-msg=0;flags=;id=66069ab2-4602-4aba-be3d-c18116305743;mod=0;returning-chatter=0;room-id=169615083;subscriber=1;tmi-sent-ts=1703554754701;turbo=0;user-id=169615083;user-type= :leostera!leostera@leostera.tmi.twitch.tv PRIVMSG #leostera :teeeest *)

type command =
  | PONG  (** PONG *)
  | CAP_REQ of capability list
      (** CAP REQ :twitch.tv/membership twitch.tv/tags twitch.tv/commafdiojfodis *)
  | PASS of string  (** PASS oauth:yfvzjqb705z12hrhy1zkwa9xt7v662 *)
  | NICK of string  (** NICK myusernmae *)
  | JOIN of string list  (** JOIN #foo,#bar *)
  | SEND_PRIVMSG of { channel : string; msg : string }
      (** PRIVMSG #leostera :ocaml my camel *)

let cmd_to_string cmd =
  match cmd with
  | PONG -> "PONG"
  | JOIN channels ->
      "JOIN " ^ String.concat "," (List.map (fun ch -> "#" ^ ch) channels)
  | CAP_REQ reqs ->
      "CAP REQ :" ^ String.concat " " (List.map capability_to_string reqs)
  | PASS pass -> "PASS oauth:" ^ pass
  | NICK nick -> "NICK " ^ nick
  | SEND_PRIVMSG { channel; msg } -> "PRIVMSG " ^ channel ^ " :" ^ msg

let rec parse carry_over lines acc =
  match lines with
  | [] -> Ok (List.rev acc, "")
  | line :: lines -> parse_one carry_over line lines acc

and parse_one carry_over line lines acc =
  match String.split_on_char ' ' line with
  | _ :: "PART" :: _channel -> parse carry_over [] (PART :: acc)
  | "PING" :: _ -> parse carry_over [] (PING :: acc)
  | _ :: "353" :: _current_user :: "=" :: channel :: users ->
      parse carry_over [] (JOINED { channel; users } :: acc)
  | meta :: user :: "PRIVMSG" :: channel :: msg ->
      parse carry_over lines (RECV_PRIVMSG { meta; user; channel; msg } :: acc)
  | _ :: "CAP" :: "*" :: "NAK" :: reqs ->
      parse carry_over lines (CAP_NAK reqs :: acc)
  | _ :: "CAP" :: "*" :: "ACK" :: reqs ->
      parse carry_over lines (CAP_ACK reqs :: acc)
  | _ :: "NOTICE" :: "*" :: msg -> parse carry_over lines (NOTICE msg :: acc)
  | _ :: "JOIN" :: channel :: users ->
      parse carry_over [] (JOINED { channel; users } :: acc)
  | [ meta; _; "USERSTATE"; channel ] ->
      parse carry_over lines (USERSTATE { meta; channel } :: acc)
  | [ meta; _; "ROOMSTATE"; channel ] ->
      parse carry_over lines (ROOMSTATE { meta; channel } :: acc)
  | _ :: _ :: _username :: ":Welcome," :: "GLHF!" :: _ ->
      parse carry_over [] (LOGIN_SUCCESSFUL :: acc)
  | _ -> Error (`Parse_error line)

let print_command cmd =
  match cmd with
  | PASS _ -> Logger.debug (fun f -> f "chatty < PASS <redacted>")
  | _ -> Logger.debug (fun f -> f "chatty < %s" (cmd_to_string cmd))
