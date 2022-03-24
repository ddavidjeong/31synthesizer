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
val new_wave : wave -> float -> synth

(* [start synth] starts the synth *)
val start : synth -> int -> int -> int -> unit

(* [release synth] sends the release signal to [synth] *)
val release : synth -> Mm_ao.writer -> Audio.t -> unit

(* [is_playing synth] returns true if [synth] is still playing *)
val is_playing : synth -> bool
val sound : float -> int -> wave -> Audio.Generator.of_mono

val write_sound :
  int -> int -> int -> int -> Audio.Generator.of_mono -> unit
