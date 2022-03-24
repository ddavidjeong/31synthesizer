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

let get_wave wave sample_rate freq =
  match wave with
  | Square -> new Audio.Mono.Generator.square sample_rate freq
  | Saw -> new Audio.Mono.Generator.saw sample_rate freq
  | Triangle -> new Audio.Mono.Generator.triangle sample_rate freq
  | Sine -> new Audio.Mono.Generator.sine sample_rate freq

let new_wave wave freq =
  {
    waveform = wave;
    frequency = freq;
    playing = true;
    sound_state = Sound_state.init_state ();
  }

let start state channels sample_rate buf_len =
  state.playing <- true;
  let ao = new Mm_ao.writer channels sample_rate in
  let buf = Audio.create channels buf_len in
  ao#write buf

let release sound (ao : Mm_ao.writer) (buf : Audio.t) =
  sound.playing <- false;
  ao#close

let is_playing sound = sound.playing = true
let time = 6
let sample_rate = 44000

let sound (freq : float) sample_rate (wave : wave) =
  (* new Audio.Generator.of_mono ((get_wave wave) sample_rate (440.0 *.
     freq)) *)
  freq |> get_wave wave sample_rate |> new Audio.Generator.of_mono

let write_sound
    channels
    sample_rate
    buf_len
    duration
    (mono_generator : Audio.Generator.of_mono) =
  let ao = new Mm_ao.writer channels sample_rate in
  let wav =
    new Audio.IO.Writer.to_wav_file channels sample_rate "out.wav"
  in
  let buf = Audio.create channels buf_len in
  for _ = 0 to (sample_rate / buf_len * duration) - 1 do
    mono_generator#fill buf;
    wav#write buf;
    ao#write buf
  done;
  wav#close;
  ao#close
