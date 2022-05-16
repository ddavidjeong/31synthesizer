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
  mutable generator : Audio.Generator.of_mono;
}

let get_wave (wave : wave) (sample_rate : int) (freq : float) =
  match wave with
  | Square -> new Audio.Mono.Generator.square sample_rate freq
  | Saw -> new Audio.Mono.Generator.saw sample_rate freq
  | Triangle -> new Audio.Mono.Generator.triangle sample_rate freq
  | Sine -> new Audio.Mono.Generator.sine sample_rate freq

let new_ao_state (channels : int) (sample_rate : int) =
  new Mm_ao.writer channels sample_rate

let new_buf (channels : int) (buf_len : int) =
  Audio.create channels buf_len

let new_wav (channels : int) (sample_rate : int) =
  new Audio.IO.Writer.to_wav_file channels sample_rate "out.wav"

let new_generator (freq : float) (sample_rate : int) (wave : wave) =
  let wave = get_wave wave sample_rate freq in
  new Audio.Generator.of_mono wave

let init_state
    (channels : int)
    (sample_rate : int)
    (buf_len : int)
    (freq : float)
    (wave : wave) =
  {
    blen = buf_len;
    channels;
    ao = new_ao_state channels sample_rate;
    buffer = new_buf channels buf_len;
    generator = new_generator freq sample_rate wave;
  }

type synth = {
  waveform : wave;
  mutable frequency : float;
  sample_rate : int;
  mutable playing : bool;
  sound_state : sound_state;
}

let get_ch sound = sound.sound_state.channels
let get_buf sound = sound.sound_state.buffer
let set_buf sound buf = sound.sound_state.buffer <- buf

let new_wave
    wave
    (freq : float)
    (sample_rate : int)
    (ch : int)
    (buf_len : int) =
  {
    waveform = wave;
    frequency = freq;
    sample_rate;
    playing = false;
    sound_state = init_state ch sample_rate buf_len freq wave;
  }

let get_waveform sound = sound.waveform
let set_freq sound (freq : float) = sound.frequency <- freq
let get_freq sound = sound.frequency
let get_sr sound = sound.sample_rate
let get_generator sound = sound.sound_state.generator

let start_generator sound =
  let buf = get_buf sound in
  let generator = get_generator sound in
  generator#fill buf;
  sound.sound_state.buffer <- buf

let start sound =
  sound.playing <- true;
  let ao = sound.sound_state.ao in
  let buf = sound.sound_state.buffer in
  (* let wave = sound.sound_state.generator in wave#fill buf; *)
  ao#write buf;
  sound.sound_state.ao <- ao

let release sound =
  sound.playing <- false;
  let ao = sound.sound_state.ao in
  ao#close

let is_playing sound = sound.playing = true