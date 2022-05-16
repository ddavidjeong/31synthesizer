open Synth
open Mm_audio
open Mm
open Filters

let channels = 4
let sample_rate = 44100
let blen = 1024
let use_envelope = ref false
let record_on = ref false

let parse (input : string) : float * int * string * string * float =
  let inputs = String.split_on_char ' ' input in
  let freq = List.nth inputs 0 |> float_of_string in
  let dur = List.nth inputs 1 |> int_of_string in
  let wave = List.nth inputs 2 in
  let filter =
    try List.nth inputs 3 with
    | _ -> "no filter"
  in
  let filter_inten =
    try List.nth inputs 4 |> float_of_string with
    | _ -> 0.
  in
  (freq, dur, wave, filter, filter_inten)

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

let get_filter_fun filter =
  use_envelope := false;
  match filter with
  | "envelope" ->
      use_envelope := true;
      fun x y -> y
  | "blur" -> Filters.blur
  | "smooth" -> Filters.smooth
  | "adsr" -> Filters.adsr
  | "range" -> Filters.range
  | _ -> Filters.iden

let playback fn = IO.open_wav fn

let rec record_sound io input =
  let total_duration = snd input in
  let frequency = fst input in
  let wave = get_wave (thd input) in
  let filter = get_filter_fun (frth input) in
  let param = fifth input in
  let sound = Sound.new_wave wave frequency sample_rate channels blen in
  let buf = Sound.get_buf sound in
  let last_index = (sample_rate / blen * total_duration) - 1 in
  for i = 0 to last_index do
    Sound.start_generator sound;
    let outbuf =
      if !use_envelope then envelope i last_index param buf
      else filter param buf
    in
    Sound.set_buf sound outbuf;
    Sound.start sound;
    Synth__IO.record sound io
  done;
  Synth__Sound.release sound;
  record_play io

and record_play io =
  print_string
    "\n\
     Please input: <frequency : float> <duration : int> <waveform : \
     string> <filter : string> <filter parameter : float>: \n\n\
     > ";
  match read_line () with
  | x when String.trim x = "stop" -> Synth__IO.stop_recording io
  | input -> input |> parse |> record_sound io

let get_generator wave =
  match wave with
  | "square" -> Sound.Square
  | "saw" -> Sound.Saw
  | "triangle" -> Sound.Triangle
  | "sine" -> Sound.Sine
  | _ -> Sound.Sine

let play_sound input =
  let freq = fst input in
  let total_duration = snd input in
  let wave1 = get_generator (thd input) in
  let filter = get_filter_fun (frth input) in
  let param = fifth input in
  let sound = Sound.new_wave wave1 freq sample_rate channels blen in
  let buf1 = Sound.get_buf sound in
  let last_index = (sample_rate / blen * total_duration) - 1 in
  for i = 0 to last_index do
    Sound.start_generator sound;
    let outbuf =
      if !use_envelope then envelope i last_index param buf1
      else filter param buf1
    in
    Sound.set_buf sound outbuf;
    Sound.start sound
  done;
  Sound.release sound

let terminal_interface =
  print_string
    "\n\
     - To play a sound, please input: <frequency : float> <duration : \
     int> <waveform : string> <filter : string> <filter parameter : \
     float>\n\
     - To play a .wav file please enter \"play\". Then, input file \
     name when prompted.\n\
     - To record, please enter \"record\". Then, input the required \
     data.\n\
     - To stop recording, enter \"stop\"\n\
     - To exit the interface, enter \"quit\"\n\n";
  while true do
    print_string "> ";
    match read_line () with
    | x when String.trim x = "quit" -> Stdlib.exit 0
    | x when String.trim x = "record" ->
        record_on := true;
        let fn =
          read_line
            (print_endline
               "\nPlease enter the name for your new file: \n")
        in
        let io = IO.init_io channels sample_rate fn in
        record_play io
    | x when String.trim x = "play" -> begin
        try
          let fn =
            read_line (print_endline "\nPlease enter your filename: \n")
          in
          playback fn
        with
        | Unix.Unix_error _ ->
            print_endline "\nThis is not a valid file name.\n"
        | Invalid_argument _ -> ()
        | _ -> ()
      end
    | input -> (
        try input |> parse |> play_sound with
        | _ ->
            print_endline
              "\nThis is not a valid command. Try again pls. \n")
  done
