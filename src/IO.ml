open Synth__Sound
open Mm
open Mm_audio

type io_t = {
  filename : string;
  mutable wav : Audio.IO.Writer.to_wav_file;
}

let init_io ch sr fn =
  let name = Printf.sprintf "%s.wav" fn in
  { filename = fn; wav = new Audio.IO.Writer.to_wav_file ch sr name }

let record sound io =
  let buf = get_buf sound in
  let wav = io.wav in
  wav#write buf;
  io.wav <- wav

let stop_recording io =
  let wav = io.wav in
  wav#close