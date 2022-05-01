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

let print_float_array a = 
  print_string "\n[| ";
  for i = 0 to Array.length a - 1 do 
    print_float a.(i); 
    print_string "; "
  done;
  print_string " |]\n"

(* Applies function f to each channel of an audio array. *)
let all_channels a f = 
  for c = 0 to Array.length a - 1 do f a.(c) done

(*  Mutates an array in-place such that each element is the average of itself
    and the next n elements. *)
let blur n a = 
  for i = 0 to Array.length a - 1 - n do
    a.(i) <- (a.(i) +. a.(i + n)) /. float_of_int n
  done

(*  Smoothing lowpass filter example adapted from:
    http://phrogz.net/js/framerate-independent-low-pass-filter.html*)
let smooth (smoothing : float) (values : float array) = 
  let value = ref values.(0) in 
  for i = 1 to Array.length values - 1 do
    let current_value = values.(i) in 
    value := !value +. ((current_value -. !value) /. smoothing);
    values.(i) <- current_value
  done

let play_using_generator input = 
  let total_duration = snd input in
  let channels = 2 in
  let sample_rate = 44100 in
  let freq = fst input in
  let ao = new Mm_ao.writer channels sample_rate in
  let wav = new Audio.IO.Writer.to_wav_file channels sample_rate "out.wav" in
  let blen = 1024 in
  let buf = Audio.create channels blen in
  let generator =
    new Audio.Generator.of_mono (new Audio.Mono.Generator.sine sample_rate freq)
  in
  let loop_indices = (sample_rate / blen * total_duration) - 1 in
  for x = 0 to loop_indices do
    generator#fill buf;
    (* Envelope *)
    (* let factor = 6 in 
    let inc_coeff = x mod (loop_indices / factor) |> abs in 
    let dec_coeff = (loop_indices - x) mod (loop_indices / factor) |> abs in 
    Audio.add_coeff buf ((inc_coeff - dec_coeff) |> abs |> float_of_int) buf;
    *)
    
    (* Avg filter *)
    
    let a = Audio.to_array buf in (* Get array of raw data *)
    blur 20 |> all_channels a; (* Smoothing using my blur *)
    (*smooth 10. |> all_channels a;*) (* Smoothing using smooth example *)
    let buf = Audio.of_array a in (* Create abstract Audio.t from array *) 
    
    wav#write buf;
    ao#write buf;
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
      generator  *)
  let play_sound input =
  (*play_using_synth input*)
  play_using_generator input

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

let play =
  while true do
    print_string
      "To play a sound, please input: <frequency : float> <duration : \
       int> <waveform : string>\n\
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

let () = play
