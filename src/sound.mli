open Mm

type wave =
  | Sine
  | Saw
  | Triangle
  | Square

type synth

(* [create waveform (wave)] create a new
 * synthesized sound with the given waveform with the given frequency
 *)
val new_wave : wave -> float -> int -> int -> int -> synth

(* [start synth] starts the synth *)
val start : synth -> unit

(* [release synth] sends the release signal to [synth] *)
val release : synth -> unit

(* [is_playing synth] returns true if [synth] is still playing *)
val is_playing : synth -> bool
val make_generator : float -> int -> wave -> Audio.Generator.of_mono

val write_sound :
  float -> int -> int -> int -> int -> int -> string -> wave -> unit
