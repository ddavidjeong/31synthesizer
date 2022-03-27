open OUnit2
open Mm_audio

let parse (input : string) : float * int * string =
  let inputs = String.split_on_char ' ' input in
  let freq = List.nth inputs 0 |> float_of_string in
  let dur = List.nth inputs 1 |> int_of_string in
  let wave = List.nth inputs 2 in
  (freq, dur, wave)

let get_wave = function
  | "square" -> Synth__Sound.Square
  | "saw" -> Synth__Sound.Saw
  | "triangle" -> Synth__Sound.Triangle
  | "sine" -> Synth__Sound.Sine
  | _ -> Synth__Sound.Sine

let fst (x, _, _) = x
let snd (_, y, _) = y
let thd (_, _, z) = z

let play_sound input =
  let total_duration = snd input in
  let frequency = fst input in
  let channels = 4 in
  let sample_rate = 2000 in
  (* let ao = new Mm_ao.writer channels sample_rate in *)
  (* let wav = new Audio.IO.Writer.to_wav_file channels sample_rate
     "../output/out.wav" in *)
  (* let wave = Synth.sound *)
  let blen = 1024 in
  let wave = get_wave (thd input) in
  Synth__Sound.write_sound frequency sample_rate channels sample_rate
    blen total_duration "out.wav" wave
(* let buf = Audio.create channels blen in let sine = new
   Audio.Generator.of_mono (get_wave (thd input) sample_rate frequency)
   in for _ = 0 to (sample_rate / blen * total_duration) - 1 do
   sine#fill buf; wav#write buf; ao#write buf done; wav#close;
   ao#close *)

let play =
  while true do
    print_string
      "Input: <frequency : float>\n\
      \   <duration : int> <waveform : string>  or \"quit\": ";
    match read_line () with
    | x when String.trim x = "quit" -> Stdlib.exit 0
    | input -> input |> parse |> play_sound
  done

let () = play
