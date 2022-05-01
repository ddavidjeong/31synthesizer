open OUnit2
open Synth
open Synth.Filters

let gui_test (name : string) (func) (input : char) (expected_output : int) :
    test = name >:: fun _ ->
  (* the [printer] tells OUnit how to convert the output to a string *)
  assert_equal expected_output (func input) ~printer:string_of_int

let generator_test (name : string) (func) (input : char) (expected_output : int) :
  test = name >:: fun _ ->
(* the [printer] tells OUnit how to convert the output to a string *)
assert_equal expected_output (func input) ~printer:string_of_int

<<<<<<< HEAD
let fst (x, _, _) = x
let snd (_, y, _) = y
let thd (_, _, z) = z

let play_using_generator input =
  let total_duration = snd input in
  let channels = 2 in
  let sample_rate = 44100 in
  let freq = fst input in
  let ao = new Mm_ao.writer channels sample_rate in
  let wav =
    new Audio.IO.Writer.to_wav_file channels sample_rate "out.wav"
  in
  let blen = 1024 in
  let buf = Audio.create channels blen in
  let sine =
    new Audio.Generator.of_mono
      (new Audio.Mono.Generator.sine sample_rate freq)
  in
  let loop_indices = (sample_rate / blen * total_duration) - 1 in
  for x = 0 to loop_indices do
    sine#fill buf;
    wav#write buf;
    ao#write buf
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
  let sound =
    Synth__Sound.new_wave wave frequency sample_rate channels blen
  in
  Synth__Sound.start sound

(* Use this to switch between playing a whole buffer or looping over a
   generator *)
let play_sound input = play_using_synth input
(*play_using_generator input*)
=======
let filter_test (name : string) (func) (input : char) (expected_output : int) :
    test = name >:: fun _ ->
  (* the [printer] tells OUnit how to convert the output to a string *)
  assert_equal expected_output (func input) ~printer:string_of_int

let gui_tests = []

let generator_tests = []
>>>>>>> fde72cf8e352e0799bc216c098e538f104d66051

let filter_tests = []

let tests =
  "test suite for Synth"
  >::: List.flatten
         [
           gui_tests;
           generator_tests;
           filter_tests
         ]

<<<<<<< HEAD
type input =
  | Record
  | Play of string

let play =
  while true do
    print_string
      "To play a sound, please input: <frequency : float> <duration : \
       int> <waveform : string>\n\
      \       To record, please enter \"record\". Then, input the \
       required data.\n\
      \       To stop recording, enter \"stop\"\n\
      \       To exit the interface, enter \"quit\"\n\
       > ";
    match read_line () with
    | x when String.trim x = "quit" -> Stdlib.exit 0
    | x when String.trim x = "record" ->
        let ch = 4 in
        let sr = 2000 in
        let io = Synth__IO.init_io ch sr "test" in
        record_play io
    | x when String.trim x = "play" ->
        let fn_input =
          read_line
            (print_endline "enter your sound file name (.wav): ")
        in
        Synth.IO.open_wav fn_input
    | input -> input |> parse |> play_sound
  done
=======
let () = 
  print_endline "What to test? ([Enter] runs suite)";
  match read_line () with 
  | "filters" -> play
  | _ -> run_test_tt_main tests
>>>>>>> fde72cf8e352e0799bc216c098e538f104d66051

