open Mm_audio
open Mm

let parse (input : string) : float * int * string * string =
  let inputs = String.split_on_char ' ' input in
  let freq = List.nth inputs 0 |> float_of_string in
  let dur = List.nth inputs 1 |> int_of_string in
  let wave = List.nth inputs 2 in
  let filter = List.nth inputs 3 in
  (freq, dur, wave, filter)

let get_wave = function
  | "square" -> Synth__Sound.Square
  | "saw" -> Synth__Sound.Saw
  | "triangle" -> Synth__Sound.Triangle
  | "sine" -> Synth__Sound.Sine
  | _ -> Synth__Sound.Sine


let fst (x, _, _, _) = x
let snd (_, x, _, _) = x
let thd (_, _, x, _) = x
let frth (_, _, _, x) = x

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

let all_channels_2 a1 a2 f = 
  for c = 0 to Array.length a1 - 1 do f a1.(c) a2.(c) done

(*  Mutates an array in-place such that each element is the average of itself
    and the next n elements. *)
let blur n a = 
  for i = 0 to Array.length a - 1 - n do
    a.(i) <- (a.(i) +. a.(i + n)) /. float_of_int n
  done

(*  Smoothing lowpass filter example adapted to OCaml from:
    http://phrogz.net/js/framerate-independent-low-pass-filter.html*)
let smooth (smoothing : float) (values : float array) = 
  for i = 0 to Array.length values - 1 do
    let current_value = values.(i) in 
    let new_value = current_value *. (smoothing ** float_of_int i) in
    values.(i) <- new_value
  done

let envelope (factor : float) (index : int) last_index buf = 
  let indexf = float_of_int index in 
  let last_indexf = float_of_int last_index in 
  let inc_coeff = mod_float indexf (last_indexf) /. factor |> Float.abs in 
  let dec_coeff = mod_float (last_indexf -. indexf) (last_indexf /. factor) |> Float.abs in 
  Audio.add_coeff buf ((inc_coeff -. dec_coeff) |> Float.abs) buf

(*let envelope (factor : int) (index : int) last_index buf = 
  let inc_coeff = index mod (last_index) / factor |> abs in 
  let dec_coeff = (last_index - index) mod (last_index / factor) |> abs in 
  Audio.add_coeff buf ((inc_coeff - dec_coeff) |> abs |> float_of_int) buf*)


let get_generator = function
  | "square" -> new Audio.Mono.Generator.square
  | "saw" -> new Audio.Mono.Generator.saw
  | "triangle" -> new Audio.Mono.Generator.triangle
  | "sine" -> new Audio.Mono.Generator.sine
  | _ -> new Audio.Mono.Generator.sine

let add_waves w1 w2 sum = 
  let max = ref 0. in
  for i = 0 to Array.length w1 - 1 do 
    let value = w1.(i) +. w2.(i) in 
    if value > !max then max := value; 
    sum.(i) <- value
  done;
  for i = 0 to Array.length w1 - 1 do 
    sum.(i) <- sum.(i) /. !max
  done

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
    
    let a1 = Audio.to_array buf1 in (* Get array of raw data *)
    let a2 = Audio.to_array buf2 in
    let sum_c1 = Array.make (Array.length a1.(0)) 0. in
    let sum_c2 = Array.make (Array.length a1.(1)) 0. in
    add_waves a1.(0) a2.(0) sum_c1;
    add_waves a1.(1) a2.(1) sum_c2;
    let sum_array = [| sum_c1; sum_c2 |] in
    
    (*blur 2 |> all_channels a;*) (* Smoothing using my blur *)
    smooth (-0.9) |> all_channels a1; (* Smoothing using smooth example *)
    (*let buf = Audio.of_array a1 in*) (* Create abstract Audio.t from array *) 
    let buf1 = Audio.of_array sum_array in

    wav#write buf1;
    ao#write buf1;
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