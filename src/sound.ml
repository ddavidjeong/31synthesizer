open Mm_audio
open Mm

type wave =
  | Sine
  | Saw
  | Triangle
  | Square

type sound_state = {
  blen : int;
  channels : int;
  mutable ao : Mm_ao.writer;
  mutable buffer : Audio.t;
}

let new_ao_state channels sample_rate =
  new Mm_ao.writer channels sample_rate

let new_buf channels buf_len = Audio.create channels buf_len

let new_wav channels sr =
  new Audio.IO.Writer.to_wav_file channels sr "out.wav"

let init_state channels sr buf_len =
  {
    blen = buf_len;
    channels;
    ao = new_ao_state channels sr;
    buffer = new_buf channels buf_len;
  }

type synth = {
  waveform : wave;
  frequency : float;
  sample_rate : int;
  mutable playing : bool;
  sound_state : sound_state;
}

let get_ch sound = sound.sound_state.channels
let get_buf sound = sound.sound_state.buffer

let get_wave wave sample_rate freq =
  match wave with
  | Square -> new Audio.Mono.Generator.square sample_rate freq
  | Saw -> new Audio.Mono.Generator.saw sample_rate freq
  | Triangle -> new Audio.Mono.Generator.triangle sample_rate freq
  | Sine -> new Audio.Mono.Generator.sine sample_rate freq

let new_wave wave freq sr ch blen =
  {
    waveform = wave;
    frequency = freq;
    sample_rate = sr;
    playing = false;
    sound_state = init_state ch sr blen;
  }

let get_waveform sound = sound.waveform
let get_freq sound = sound.frequency
let get_sr sound = sound.sample_rate

let make_generator (freq : float) sample_rate (wave : wave) =
  (* new Audio.Generator.of_mono ((get_wave wave) sample_rate (440.0 *.
     freq)) *)
  freq |> get_wave wave sample_rate |> new Audio.Generator.of_mono

let start sound =
  sound.playing <- true;
  let ao = sound.sound_state.ao in
  let buf = sound.sound_state.buffer in
  let wave =
    make_generator sound.frequency sound.sample_rate sound.waveform
  in
  wave#fill buf;
  ao#write buf;
  sound.sound_state.ao <- ao

let release sound =
  sound.playing <- false;
  let ao = sound.sound_state.ao in
  ao#close

let is_playing sound = sound.playing = true

let write_sound
    freq
    sr
    channels
    buf_len
    duration
    (file_name : string)
    wave =
  let sound = new_wave wave freq sr channels buf_len in
  for _ = 0 to (sr / buf_len * duration) - 1 do
    start sound
  done;
  release sound