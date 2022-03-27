open Graphics
open Printf
(* open Sound *)

let sample_rate = 1000
let channels = 4
let buf_len = 1024

let key_pressed e =
  (* let dur = 1 in *)
  match e.key with
  | 'q' ->
      let freq = 261. in
      let wave =
        Sound.new_wave Sound.Sine freq sample_rate channels buf_len
      in
      Sound.start wave;
      if e.keypressed = false then Sound.release wave
  | 'w' ->
      let freq = 293. in
      let wave =
        Sound.new_wave Sound.Sine freq sample_rate channels buf_len
      in
      Sound.start wave;
      if e.keypressed = false then Sound.release wave
  | 'e' ->
      let freq = 329. in
      let wave =
        Sound.new_wave Sound.Sine freq sample_rate channels buf_len
      in
      Sound.start wave;
      if e.keypressed = false then Sound.release wave
  | 'r' ->
      let freq = 349. in
      let wave =
        Sound.new_wave Sound.Sine freq sample_rate channels buf_len
      in
      Sound.start wave;
      if e.keypressed = false then Sound.release wave
  | 't' ->
      let freq = 392. in
      let wave =
        Sound.new_wave Sound.Sine freq sample_rate channels buf_len
      in
      Sound.start wave;
      if e.keypressed = false then Sound.release wave
  | _ -> printf "Try one of the other keys!"
