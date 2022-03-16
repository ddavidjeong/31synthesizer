open Mm_audio
open Mm

type wave =
  | Sine
  | Saw
  | Triangle
  | Square

type synth = {
  waveform : wave;
  frequency : float;
  mutable playing : bool;
  sound_state : Sound_state.sound_state;
}

let new_wave wave freq =
  {
    waveform = wave;
    frequency = freq;
    playing = true;
    sound_state = Sound_state.init_state ();
  }

let start state = state.playing <- true
let release sound = sound.playing <- false
let is_playing sound = sound.playing = true
let time = 6
let sample_rate = 44000

let sine_sound (freq : float) =
  new Audio.Generator.of_mono
    (new Audio.Mono.Generator.sine sample_rate (440.0 *. freq))
