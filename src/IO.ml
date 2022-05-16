open Synth__Sound
open Mm
open Mm_audio

type io_t = {
  filename : string;
  mutable wav : Audio.IO.Writer.to_wav_file;
}

let init_io (channels : int) (sample_rate : int) (filename : string) =
  let name = Printf.sprintf "%s.wav" filename in
  {
    filename;
    wav = new Audio.IO.Writer.to_wav_file channels sample_rate name;
  }

let get_filename io = io.filename

let record sound io =
  let buf = get_buf sound in
  let wav = io.wav in
  wav#write buf;
  io.wav <- wav

let stop_recording io =
  let wav = io.wav in
  wav#close

let open_wav fn =
  let f = new Audio.IO.Reader.of_wav_file fn in
  let channels = f#channels in
  let sr = f#sample_rate in
  let duration = int_of_float (ceil f#duration) in
  let blen = 1024 in
  let ao = new Mm_ao.writer channels sr in
  let buf = Audio.create f#channels blen in
  Printf.printf "\nPlayed WAV file named: %s!\n\n" fn;
  let loop_end =
    if duration = 1 then (sr / blen * duration) - 1
    else sr / blen * duration
  in
  for _ = 0 to loop_end do
    let r = f#read buf in
    ao#write (Audio.sub buf 0 r)
  done;
  ao#close;
  f#close