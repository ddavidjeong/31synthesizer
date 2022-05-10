open Sound

type io_t
(** input/output type *)

val init_io : int -> int -> string -> io_t
(** [init_io ch sr fn] initializes an io type with channels [ch], sample
    rate [sr], and a file name [fn]*)

val get_filename : io_t -> string
(** [get_filename io] gets the file name [fn] that is associated with
    the io type *)

val record : synth -> io_t -> unit
(** [record sound io] starts the recording of the sound buffer on the io
    type*)

val stop_recording : io_t -> unit
(** [stop_recording io] ends the recording and closes the .wav file
    associated with the recorded sounds. If the recording is not
    stopped, then the .wav file will be incomplete and will not be
    playable*)

val open_wav : string -> unit
(** [open_wav fn] plays the .wav file with the associated file name [fn]*)