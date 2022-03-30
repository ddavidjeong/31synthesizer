open Synth__Sound

type io_t

val init_io : int -> int -> string -> io_t
val record : synth -> io_t -> unit
val stop_recording : synth -> io_t -> unit

(* val record : synth -> string -> unit *)
(** [record synth fn] creates a new .wav file under fn and starts
    recording a sound*)

(* val stop_recording : synth -> unit *)
(** [stop_recording synth] stops a .wav file from recording*)