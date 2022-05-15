open Mm_audio
open Mm

(* let parse (input : string) : float * int * string * string = let
   inputs = String.split_on_char ' ' input in let freq = List.nth inputs
   0 |> float_of_string in let dur = List.nth inputs 1 |> int_of_string
   in let wave = List.nth inputs 2 in let filter = List.nth inputs 3 in
   (freq, dur, wave, filter)

   let get_wave = function | "square" -> Sound.Square | "saw" ->
   Sound.Saw | "triangle" -> Sound.Triangle | "sine" -> Sound.Sine | _
   -> Sound.Sine

   let fst (x, _, _, _) = x let snd (_, x, _, _) = x let thd (_, _, x,
   _) = x let frth (_, _, _, x) = x *)

let print_float_array a =
  print_string "\n[| ";
  for i = 0 to Array.length a - 1 do
    print_float a.(i);
    print_string "; "
  done;
  print_string " |]\n"

(* Applies function f to each channel of an audio array. *)
<<<<<<< Updated upstream
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
=======
let all_channels array func =
  for c = 0 to Array.length array - 1 do
    func array.(c)
  done
  
  let blur (inten : int) (values : float array) =
  assert (inten <= Array.length values);
  for i = 0 to Array.length values - 1 - inten do
    values.(i) <-
      values.(i) +. (values.(i + inten) /. float_of_int inten)
  done

let range (inten : float) (values : float array) =
  let sine_float = 400. in
  let old_value = inten in
  let old_min = 0.0 in
  let old_max = 10.0 in
  let new_min = 0.8 in
  let new_max = 1.2 in
  let new_value =
    (old_value -. old_min) /. (old_max -. old_min)
    *. (new_max -. new_min)
    +. new_min
  in
  for i = 0 to Array.length values - 1 do
    values.(i) <- new_value *. sine_float
  done

(* Smoothing lowpass filter example adapted from:
   http://phrogz.net/js/framerate-independent-low-pass-filter.html*)
let smooth (smoothing : float) (values : float array) =
  let value = ref values.(0) in
  for i = 1 to Array.length values - 1 do
    let current_value = values.(i) in
    value := !value +. ((current_value -. !value) /. smoothing);
    values.(i) <- current_value
  done

let new_val
    (ovalue : float)
    (omin : float)
    (omax : float)
    (nmin : float)
    (nmax : float) =
  ((ovalue -. omin) /. (omax -. omin) *. (nmax -. nmin)) +. nmin

let attack (inten : float) (values : float array) =
  for i = 1 to Array.length values - 1 do
    values.(i) <-
      values.(i) *. values.(i) *. new_val inten 0.0 1.0 1.0 2.0
  done

let decay (inten : float) (values : float array) =
  for i = 1 to Array.length values - 1 do
    values.(i) <-
      values.(i)
      *. (1.0 -. new_val inten 0.0 1.0 0.0 0.8)
         ** float_of_int (Array.length values)
  done

let release (inten : float) (values : float array) =
  for i = 1 to Array.length values - 1 do
    values.(i) <-
      values.(i)
      *. (1.0 -. new_val inten 0.0 1.0 0.0 0.65)
         ** float_of_int (Array.length values)
  done

let adsr (inten : float) (values : float array) =
  for i = 1 to Array.length values / 4 do
    values.(i) <-
      values.(i) *. values.(i) *. new_val inten 0.0 1.0 1.0 2.0
  done;
  for i = (Array.length values / 4) + 1 to Array.length values / 2 do
    values.(i) <-
      values.(i)
      *. (1.0 -. new_val inten 0.0 1.0 0.0 0.8)
         ** float_of_int (Array.length values)
  done;
  for
    i = (Array.length values / 2) + 1
    to (Array.length values / 2) + (Array.length values / 4)
  do
    values.(i) <- values.(i)
  done;
  for
    i = (Array.length values / 2) + (Array.length values / 4) + 1
    to Array.length values - 1
  do
    values.(i) <-
      values.(i)
      *. (1.0 -. new_val inten 0.0 1.0 0.0 0.65)
         ** float_of_int (Array.length values)
  done

(* Mutates an array in-place such that each element is the average of
   itself and the next n elements. *)
let blur (n : float) buf =
  let arr = Audio.to_array buf in
  let blur_func a =
    for i = 0 to Array.length a - 1 - int_of_float n do
      a.(i) <- a.(i) +. (a.(i + int_of_float n) /. n)
    done
  in
  all_channels arr blur_func;
  let buf = Audio.of_array arr in
  buf
(* in all_channels full_arr blur_func; buf *)

(* Smoothing lowpass filter example adapted to OCaml from:
   http://phrogz.net/js/framerate-independent-low-pass-filter.html*)
let smooth (smoothing : float) buf =
  let arr = Audio.to_array buf in
  let smooth_func values =
    let value = ref values.(0) in
    for i = 0 to Array.length values - 1 do
      let current_value = values.(i) in
      value := !value +. ((current_value -. !value) /. smoothing);
      values.(i) <- !value
    done
  in
  all_channels arr smooth_func;
  let buf = Audio.of_array arr in
  buf

(* let smooth (smoothing : float) (sound : Sound.synth) = let buf =
   Sound.get_buf sound in let values = Audio.to_array buf in let value =
   ref values.(0) in for i = 0 to Array.length values - 1 do let
   current_value = values.(i) in value := !value +. ((current_value -.
   !value) /. smoothing); values.(i) <- current_value done *)

let envelope (index : int) (last_index : int) (factor : float) buf =
  let indexf = float_of_int index in
  let last_indexf = float_of_int last_index in
  let inc_coeff = mod_float indexf last_indexf /. factor |> Float.abs in
  let dec_coeff =
    mod_float (last_indexf -. indexf) (last_indexf /. factor)
    |> Float.abs
  in
  Audio.add_coeff buf (inc_coeff -. dec_coeff |> Float.abs) buf;
  buf
>>>>>>> Stashed changes

(* let envelope (factor : int) (index : int) last_index buf = let
   inc_coeff = index mod last_index / factor |> abs in let dec_coeff =
   (last_index - index) mod (last_index / factor) |> abs in
   Audio.add_coeff buf (inc_coeff - dec_coeff |> abs |> float_of_int)
   buf *)

let get_generator = function
  | "square" -> new Audio.Mono.Generator.square
  | "saw" -> new Audio.Mono.Generator.saw
  | "triangle" -> new Audio.Mono.Generator.triangle
  | "sine" -> new Audio.Mono.Generator.sine
  | _ -> new Audio.Mono.Generator.sine

<<<<<<< Updated upstream
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
=======
(* let play_using_generator input = let total_duration = snd input in
   let channels = 2 in let sample_rate = 44100 in let frequency = fst
   input in let wav = new Audio.IO.Writer.to_wav_file channels
   sample_rate "out.wav" in let blen = 1024 in let wave = get_wave (thd
   input) in (* let sound = Synth__Sound.new_wave wave frequency
   sample_rate channels blen in *) (* let buf = Sound.get_buf sound in
   *) (* let filter = (frth input) in *) (* let generator = new
   Audio.Generator.of_mono (wave sample_rate freque) in *) let ao = new
   Mm_ao.writer channels sample_rate in let wav = new
   Audio.IO.Writer.to_wav_file channels sample_rate "out.wav" in let
   blen = 1024 in let buf = Audio.create channels blen in let wave =
   get_generator (thd input) in (* let filter = (frth input) in *) let
   generator = new Audio.Generator.of_mono (wave sample_rate frequency)
   in let last_index = (sample_rate / blen * total_duration) - 1 in for
   i = 0 to last_index do (* Envelope *) (* envelope 10. i last_index
   buf; *)

   (* Avg filter *) let a = Audio.to_array buf in (* Get array of raw
   data *) (*blur 2 |> all_channels a;*) (* Smoothing using my blur *)
   (* smooth 5. |> all_channels a; *) (* Smoothing using smooth example
   *) let buf = Audio.of_array a in

   (* Create abstract Audio.t from array *) wav#write buf (* Sound.start
   sound *) done; wav#close *)
(* Sound.release sound *)

(* let play_using_synth input = let total_duration = snd input in let
   frequency = fst input in let channels = 4 in let sample_rate = 44100
   in let blen = sample_rate * total_duration in let wave = get_wave
   (thd input) in let sound = Synth__Sound.new_wave wave frequency
   sample_rate channels blen in Synth__Sound.start sound *)

(* Use this to switch between playing a whole buffer or looping over a
   generator

   So apparently Synth just uses generator *)
>>>>>>> Stashed changes
