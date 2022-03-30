open Mm

type wave =
  | Sine
  | Saw
  | Triangle
  | Square

type synth

val new_wave : wave -> float -> int -> int -> int -> synth
(** [create waveform (wave)] create a new * synthesized sound with the
    given waveform with the given frequency *)

val start : synth -> unit
(** [start synth] starts the synth sound *)

val release : synth -> unit
(** [release synth] sends the release signal to [synth] *)

val is_playing : synth -> bool
(** [is_playing synth] returns true if [synth] is still playing *)

val make_generator : float -> int -> wave -> Audio.Generator.of_mono
(** [make_generator freq sr wave] creates a monodic generator from the
    specified wave*)

val write_sound :
  float -> int -> int -> int -> int -> string -> wave -> unit
(** [write_sound freq sr ch blen dur fn wave] outputs a sound and a .wav
    file depending on specified wave*)

val get_ch : synth -> int
val get_buf : synth -> Audio.t
val get_waveform : synth -> wave
val get_freq : synth -> float
val get_sr : synth -> int
