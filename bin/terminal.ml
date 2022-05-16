open Synth
open Mm_audio
open Mm
open Filters

let parse (input : string) : float * int * string * string =
  let inputs = String.split_on_char ' ' input in
  let freq = List.nth inputs 0 |> float_of_string in
  let dur = List.nth inputs 1 |> int_of_string in
  let wave = List.nth inputs 2 in
  let filter = List.nth inputs 3 in
  (freq, dur, wave, filter)

let fst (x, _, _, _, _) = x
let snd (_, x, _, _, _) = x
let thd (_, _, x, _, _) = x
let frth (_, _, _, x, _) = x
let fifth (_, _, _, _, x) = x

let get_wave = function
  | "square" -> Synth__Sound.Square
  | "saw" -> Synth__Sound.Saw
  | "triangle" -> Synth__Sound.Triangle
  | "sine" -> Synth__Sound.Sine
  | _ -> Synth__Sound.Sine

let rec record_sound io input =
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
  let sound =
    Synth__Sound.new_wave wave frequency sample_rate channels blen
  in
  for _ = 0 to sample_rate / blen * total_duration do
    Synth__Sound.start sound;
    Synth__IO.record sound io
  done;
  Synth__Sound.release sound;
  record_play io

and record_play io =
  print_string
    "Input: <frequency : float> <duration : int> <waveform : string>  \
     or \"quit\": ";
  match read_line () with
  | x when String.trim x = "stop" -> Synth__IO.stop_recording io
  | input -> input |> parse |> record_sound io


let get_generator = function
  | "square" -> new Audio.Mono.Generator.square
  | "saw" -> new Audio.Mono.Generator.saw
  | "triangle" -> new Audio.Mono.Generator.triangle
  | "sine" -> new Audio.Mono.Generator.sine
  | _ -> new Audio.Mono.Generator.sine


let play_using_generator input = 
    let total_duration = snd input in
    let channels = 2 in
    let sample_rate = 44100 in
    let freq = fst input in
    let ao = new Mm_ao.writer channels sample_rate in
    let wav = new Audio.IO.Writer.to_wav_file channels sample_rate "out.wav" in
    let blen = 1024 in
    let buf1 = Audio.create channels blen in
    let buf2 = Audio.create channels blen in
    let wave1 = get_generator (thd input) in
    let wave2 = get_generator "square" in
    (* let filter = (frth input) in *)
    let generator1 =
      new Audio.Generator.of_mono (wave1 sample_rate freq)
    in
    let generator2 =
      new Audio.Generator.of_mono (wave2 sample_rate freq)
    in
    let last_index = (sample_rate / blen * total_duration) - 1 in
    for i = 0 to last_index do
      generator1#fill buf1;
      generator2#fill buf2;
      (* Envelope *)
      (*envelope 10. i last_index buf;*)
  

  
      (* Avg filter *)
      
     (* let outbuf = blur (2.) buf1 Smoothing using my blur *)
     (* let outbuf = adsr (1.0) buf1 *)
     (* let outbuf = range 1.0 buf1 *)
     let outbuf = smooth (-0.9) buf1  
     (* Smoothing using smooth example *)
      
      (*let buf = Audio.of_array a1 in*) (* Create abstract Audio.t from array *) 
      in
  
      wav#write outbuf;
      ao#write outbuf;
    done;
    wav#close;
    ao#close
  
let play_using_synth input = 
    let total_duration = snd input in
    let frequency = fst input in
    let channels = 4 in
    let sample_rate = 44100 in
    let blen = sample_rate * total_duration in
    let wave = get_wave (thd input) in
    let sound = Synth__Sound.new_wave wave frequency sample_rate channels blen in
    Synth__Sound.start sound
  
    (*  Use this to switch between playing a whole buffer or looping over a 
        generator  
        
        So apparently Synth just uses generator *)
let play_sound input =
    (*play_using_synth input*)
  play_using_generator input

let terminal_interface =
    while true do
      print_string
        "To play a sound, please input: <frequency : float> <duration : \
         int> <waveform : string> <filter : string> \n\
        \       To record, please enter \"record\". Then, input the \
         required data.\n\
        \       To stop recording, enter \"stop\"\n\
        \       To exit the interface, enter \"quit\"\n> ";
      match read_line () with
      | x when String.trim x = "quit" -> Stdlib.exit 0
      | "record" ->
          let ch = 4 in
          let sr = 2000 in
          let io = Synth__IO.init_io ch sr "test" in
          record_play io
      | input -> input |> parse |> play_sound
    done