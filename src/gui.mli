open Sdlevent

type button

val create_button : int -> int -> button
val get_clicked : button -> bool
val get_x : button -> int
val get_y : button -> int
val set_clicked : button -> bool -> unit
val key_pressed : keyboard_event -> string -> Sound.synth
val letters : Sdltype.renderer -> char -> int -> int -> unit
