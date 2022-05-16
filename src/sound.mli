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

(* val make_generator : float -> int -> wave -> Audio.Generator.of_mono
   (** [make_generator freq sr wave] creates a monodic generator from
   the specified wave*) *)

val get_generator : synth -> Audio.Generator.of_mono
(** [get_generator synth] is the generator of synth [synth]*)

val start_generator : synth -> unit
(** [start_generator synth] fills the buffer of synth [synth] using the
    generator associated with synth [synth]. It then sets the buffer of
    synth [synth] to the new filler buf. *)

val get_ch : synth -> int
(** [get_ch synth] is the number of channels in synth [synth]. *)

val get_buf : synth -> Audio.t
(** [get_buf synth] is the mutable buffer of synth [synth]. *)

val set_buf : synth -> Audio.t -> unit
(** [set_buf synth buf] mutates the buffer of synth [synth] to be buffer
    [buff]. *)

val get_waveform : synth -> wave
(** [get_waveform synth] is the waveform of synth [synth]. *)

val get_freq : synth -> float
(** [get_freq synth] is the frequence of synth [synth]. *)

val get_sr : synth -> int
(** [get_sr synth] is the sample rate of synth [synth]. *)

val set_freq : synth -> float -> unit
(** [get_freq synth freq] mutates the frequency of synth [synth] to be
    float [freq]. *)
