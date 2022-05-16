open Mm_audio
open Mm

(* let parse (input : string) : float * int * string * string = let
   inputs = String.split_on_char ' ' input in let freq = List.nth inputs
   0 |> float_of_string in let dur = List.nth inputs 1 |> int_of_string
   in let wave = List.nth inputs 2 in let filter = List.nth inputs 3 in
   (freq, dur, wave, filter)*)

let get_wave = function
  | "square" -> Sound.Square
  | "saw" -> Sound.Saw
  | "triangle" -> Sound.Triangle
  | "sine" -> Sound.Sine
  | _ -> Sound.Sine

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
  for c = 0 to Array.length a - 1 do
    f a.(c)
  done

let all_channels_2 a1 a2 f =
  for c = 0 to Array.length a1 - 1 do
    f a1.(c) a2.(c)
  done

(* Mutates an array in-place such that each element is the average of
   itself and the next n elements. *)
(* let blur n a = for i = 0 to Array.length a - 1 - n do a.(i) <- (a.(i)
   +. a.(i + n)) /. float_of_int n done *)

(* Smoothing lowpass filter example adapted to OCaml from:
   http://phrogz.net/js/framerate-independent-low-pass-filter.html*)
(* let smooth (smoothing : float) (values : float array) = for i = 0 to
   Array.length values - 1 do let current_value = values.(i) in let
   new_value = current_value *. (smoothing ** float_of_int i) in
   values.(i) <- new_value done *)

let envelope (factor : float) (index : int) last_index buf =
  let indexf = float_of_int index in
  let last_indexf = float_of_int last_index in
  let inc_coeff = mod_float indexf last_indexf /. factor |> Float.abs in
  let dec_coeff =
    mod_float (last_indexf -. indexf) (last_indexf /. factor)
    |> Float.abs
  in
  Audio.add_coeff buf (inc_coeff -. dec_coeff |> Float.abs) buf

(*let envelope (factor : int) (index : int) last_index buf = let
  inc_coeff = index mod (last_index) / factor |> abs in let dec_coeff =
  (last_index - index) mod (last_index / factor) |> abs in
  Audio.add_coeff buf ((inc_coeff - dec_coeff) |> abs |> float_of_int)
  buf*)
let all_channels array func =
  for c = 0 to Array.length array - 1 do
    func array.(c)
  done

let range (inten : float) buf =
  let arr = Audio.to_array buf in
  let range (values : float array) =
    let sine_float = 0.95 in
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
      values.(i) <- values.(i) *. new_value *. sine_float
    done
  in
  all_channels arr range;
  let buf = Audio.of_array arr in
  buf

(* Smoothing lowpass filter example adapted from:
   http://phrogz.net/js/framerate-independent-low-pass-filter.html*)

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

let adsr (inten : float) buf =
  let arr = Audio.to_array buf in
  let adsr (values : float array) =
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
  in
  all_channels arr adsr;
  let buf = Audio.of_array arr in
  buf

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

let add_waves buf1 buf2 =
  let sum = [||] in
  let max = ref 0. in
  for i = 0 to Array.length buf1 - 1 do
    let value = buf1.(i) +. buf2.(i) in
    if value > !max then max := value;
    sum.(i) <- value
  done;
  for i = 0 to Array.length buf1 - 1 do
    sum.(i) <- sum.(i) /. !max
  done
