open Graphics
open Printf
(* open Sound *)

let sample_rate = 1000
let channels = 4
let buf_len = 1024
let sounds = ref []

let key_pressed e =
  (* let dur = 1 in *)
  (* if e.keypressed then *)
  match e.key with
  | 'a' ->
      let freq = 261. in
      let wave =
        Sound.new_wave Sound.Sine freq sample_rate channels buf_len
      in
      Sound.start wave;
      if e.keypressed = false then Sound.release wave
  | 's' ->
      let freq = 293. in
      let wave =
        Sound.new_wave Sound.Sine freq sample_rate channels buf_len
      in
      Sound.start wave;
      if e.keypressed = false then Sound.release wave
  | 'd' ->
      let freq = 329. in
      let wave =
        Sound.new_wave Sound.Sine freq sample_rate channels buf_len
      in
      Sound.start wave;
      if e.keypressed = false then Sound.release wave
  | 'f' ->
      let freq = 349. in
      let wave =
        Sound.new_wave Sound.Sine freq sample_rate channels buf_len
      in
      Sound.start wave;
      if e.keypressed = false then Sound.release wave
  | 'g' ->
      let freq = 392. in
      let wave =
        Sound.new_wave Sound.Sine freq sample_rate channels buf_len
      in
      Sound.start wave;
      if e.keypressed = false then Sound.release wave
  | 'h' ->
      let freq = 440. in
      let wave =
        Sound.new_wave Sound.Sine freq sample_rate channels buf_len
      in
      Sound.start wave;
      if e.keypressed = false then Sound.release wave
  | 'j' ->
      let freq = 494. in
      let wave =
        Sound.new_wave Sound.Sine freq sample_rate channels buf_len
      in
      Sound.start wave;
      if e.keypressed = false then Sound.release wave
  | 'k' ->
      let freq = 523. in
      let wave =
        Sound.new_wave Sound.Sine freq sample_rate channels buf_len
      in
      Sound.start wave;
      if e.keypressed = false then Sound.release wave
  | _ -> begin
      match !sounds with
      | (ch, wv) :: _ ->
          print_string "um";
          Sound.release wv
      | _ -> Printf.printf ""
    end
(* else *)

let make_mouse_button x y w h e =
  set_color black;
  draw_rect x y w h;
  if
    e.button && x < e.mouse_x
    && e.mouse_x < x + w
    && y < e.mouse_y
    && e.mouse_y < y + h
  then (
    set_color black;
    fill_rect x y w h)
