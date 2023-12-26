open Riot

type message = { channel : string; user : string; msg : string list }
type Message.t += Message of message | Login_successful

val name : string
val add_handler : Pid.t -> unit
val start : unit -> (Pid.t, [> `Supervisor_error ]) result
val send_message : channel:string -> msg:string -> unit
val authenticate : ?pass:string -> nick:string -> unit -> unit
val join : string list -> unit
val validate_requirements : ?reqs:Irc.capability list -> unit -> unit
