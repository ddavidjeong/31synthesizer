open Sdlevent

type button
(** type of clickable buttons *)

type slider
(** type of sliders *)

type color =
  | Black
  | White
  | Red  (** different colors yay*)

val create_button : int -> int -> int -> int -> button
(** [create_button x y w h] instantiates a new button of width [w] and
    height [h] with the upper left corner being at coordinates (x, y) on
    the GUI. The button has clicked set to false by default. *)

val create_slider : int -> int -> button -> slider
(** [create_slider x y button] instantiates a new slider associated
    button [button] with the upper left corner being at coordinates (x,
    y) on the GUI window. The slider has a default width of 110. The
    notch is set to position (x, y) by default. *)

val get_clicked : button -> bool
(** [get_clicked button] is whether button [button] is clicked. *)

val get_x : button -> int
(** [get_x button] is the x coordinate of the bottom left corner of
    button [button]. *)

val get_y : button -> int
(** [get_y button] is the y coordinate of the bottom left corner of
    button [button]. *)

val get_w : button -> int
(** [get_w button] is the width in pixels of button [button]. *)

val get_h : button -> int
(** [get_h button] is the height in pixels of button [button]. *)

val get_color : button -> int * int * int
(** [get_color button] is the color of button [button] in RGB, that is
    (int, int, int). *)

val get_x_slider : slider -> int
(** [get_x_slider slider] is the x coordinate of the bottom left corner
    of slider [slider]. *)

val get_y_slider : slider -> int
(** [get_y_slider slider] is the y coordinate of the bottom left corner
    of slider [slider]. *)

val get_notch : slider -> int * int
(** [get_notch slider] is a tuple of the (x, y) position of the notch
    associated with slider [slider]. *)

val get_corr_button : slider -> button
(** [get_corr_button slider] is the button associated with slider
    [slider]. *)

val set_clicked : button -> bool -> unit
(** [set_clicked button b] sets the clicked field of button [button] to
    [b]. *)

val set_color : button -> color -> unit
(** [set_color button color] sets the color of button [button] to
    [color]. *)

val set_notch : int -> slider -> unit
(** [set_notch x slider] sets the x position of the notch associated
    with slider [slider] to [x]. *)

val key_pressed : keyboard_event -> string -> Sound.synth
(** [key_pressed key waveform] is resulting synth with a frequency
    associated with key [key] and with waveform [waveform]. *)

val letters : Sdltype.renderer -> char -> color -> int -> int -> unit
(** [letters renderer character x y] draws on the GUI character
    [character] with the top left of the character at position (x, y). *)
