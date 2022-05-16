open Sdlevent
open Printf
open Sdl
(* open Sound *)

type color =
  | Black
  | White
  | Red

type button = {
  x : int;
  y : int;
  w : int;
  h : int;
  mutable clicked : bool;
  mutable color : color;
}

type slider = {
  x : int;
  y : int;
  length : float;
  mutable notch_pos : int * int;
  corr_button : button;
}

let match_color (color : color) =
  match color with
  | Black -> (0, 0, 0)
  | White -> (255, 255, 255)
  | Red -> (252, 3, 3)

let letters renderer (character : char) (color : color) =
  let color_int = match_color color in
  Sdlrender.set_draw_color renderer color_int 255;
  match character with
  | 'a' ->
      fun x y ->
        Sdlrender.draw_rects renderer
          [|
            { x; y; w = 2; h = 10 };
            { x; y; w = 5; h = 2 };
            { x; y = y + 3; w = 5; h = 2 };
            { x = x + 5; y; w = 2; h = 10 };
          |]
  | 'b' ->
      fun x y ->
        Sdlrender.draw_rects renderer
          [|
            { x; y; w = 2; h = 10 };
            { x = x + 5; y = y + 5; w = 2; h = 5 };
            { x = x + 3; y; w = 2; h = 5 };
            { x; y; w = 3; h = 2 };
            { x; y = y + 4; w = 7; h = 2 };
            { x; y = y + 8; w = 5; h = 2 };
          |]
  | 'c' ->
      fun x y ->
        Sdlrender.draw_rects renderer
          [|
            { x; y; w = 2; h = 10 };
            { x; y; w = 7; h = 2 };
            { x; y = y + 8; w = 7; h = 2 };
          |]
  | 'd' ->
      fun x y ->
        Sdlrender.draw_rects renderer
          [|
            { x; y; w = 2; h = 10 };
            { x; y; w = 3; h = 2 };
            { x; y = y + 8; w = 3; h = 2 };
            { x = x + 5; y = y + 3; w = 1; h = 3 };
          |];
        Sdlrender.draw_lines renderer
          [|
            (x + 3, y);
            (x + 5, y + 2);
            (x + 3, y + 1);
            (x + 5, y + 3);
            (x + 3, y + 9);
            (x + 5, y + 7);
            (x + 3, y + 8);
            (x + 5, y + 6);
          |]
  | 'e' ->
      fun x y ->
        Sdlrender.draw_rects renderer
          [|
            { x; y; w = 2; h = 10 };
            { x; y; w = 7; h = 2 };
            { x; y = y + 8; w = 7; h = 2 };
            { x; y = y + 4; w = 5; h = 2 };
          |]
  | 'f' ->
      fun x y ->
        Sdlrender.draw_rects renderer
          [|
            { x; y; w = 2; h = 10 };
            { x; y; w = 7; h = 2 };
            { x; y = y + 4; w = 5; h = 2 };
          |]
  | 'g' ->
      fun x y ->
        Sdlrender.draw_rects renderer
          [|
            { x; y; w = 2; h = 10 };
            { x; y; w = 7; h = 2 };
            { x; y = y + 8; w = 7; h = 2 };
            { x = x + 5; y = y + 5; w = 2; h = 5 };
            { x = x + 3; y = y + 4; w = 4; h = 2 };
          |]
  | 'h' ->
      fun x y ->
        Sdlrender.draw_rects renderer
          [|
            { x; y; w = 2; h = 10 };
            { x = x + 5; y; w = 2; h = 10 };
            { x; y = y + 4; w = 5; h = 2 };
          |]
  | 'i' ->
      fun x y ->
        Sdlrender.draw_rects renderer
          [|
            { x = x + 2; y; w = 2; h = 10 };
            { x; y; w = 6; h = 2 };
            { x; y = y + 8; w = 6; h = 2 };
          |]
  | 'j' ->
      fun x y ->
        Sdlrender.draw_rects renderer
          [|
            { x = x + 3; y; w = 2; h = 10 };
            { x; y = y + 8; w = 3; h = 2 };
            { x; y; w = 7; h = 2 };
          |]
  | 'k' ->
      fun x y ->
        Sdlrender.draw_rects renderer [| { x; y; w = 2; h = 10 } |];
        Sdlrender.draw_lines renderer
          [|
            (x, y + 5);
            (x + 7, y);
            (x + 7, y + 1);
            (x, y + 6);
            (x + 7, y + 10);
            (x + 7, y + 9);
            (x, y + 5);
          |]
  | 'l' ->
      fun x y ->
        Sdlrender.draw_rects renderer
          [| { x; y; w = 2; h = 10 }; { x; y = y + 8; w = 7; h = 2 } |]
  | 'm' ->
      fun x y ->
        Sdlrender.draw_rects renderer
          [| { x; y; w = 2; h = 10 }; { x = x + 7; y; w = 2; h = 10 } |];
        Sdlrender.draw_lines renderer
          [|
            (x + 4, y + 4);
            (x + 7, y + 1);
            (x + 7, y);
            (x + 4, y + 3);
            (x, y);
            (x, y + 1);
            (x + 4, y + 3);
          |]
  | 'n' ->
      fun x y ->
        Sdlrender.draw_rects renderer
          [| { x; y; w = 2; h = 10 }; { x = x + 5; y; w = 2; h = 10 } |];
        Sdlrender.draw_lines renderer
          [| (x, y); (x + 6, y + 8); (x + 6, y + 9); (x, y + 1) |]
  | 'o' ->
      fun x y ->
        Sdlrender.draw_rects renderer
          [|
            { x; y; w = 2; h = 10 };
            { x = x + 5; y; w = 2; h = 10 };
            { x; y; w = 7; h = 2 };
            { x; y = y + 8; w = 7; h = 2 };
          |]
  | 'p' ->
      fun x y ->
        Sdlrender.draw_rects renderer
          [|
            { x; y; w = 2; h = 10 };
            { x = x + 5; y; w = 2; h = 5 };
            { x; y; w = 7; h = 2 };
            { x; y = y + 5; w = 7; h = 2 };
          |]
  | 'q' ->
      fun x y ->
        Sdlrender.draw_rects renderer
          [|
            { x; y; w = 2; h = 10 };
            { x = x + 5; y; w = 2; h = 10 };
            { x; y; w = 7; h = 2 };
            { x; y = y + 8; w = 9; h = 2 };
          |]
  | 'r' ->
      fun x y ->
        Sdlrender.draw_rects renderer
          [|
            { x; y; w = 2; h = 10 };
            { x = x + 5; y; w = 2; h = 5 };
            { x; y; w = 7; h = 2 };
            { x; y = y + 4; w = 7; h = 2 };
          |];
        Sdlrender.draw_lines renderer
          [|
            (x + 2, y + 4);
            (x + 7, y + 9);
            (x + 6, y + 9);
            (x + 1, y + 4);
          |]
  | 's' ->
      fun x y ->
        Sdlrender.draw_rects renderer
          [|
            { x; y; w = 5; h = 2 };
            { x; y = y + 4; w = 5; h = 2 };
            { x; y = y + 8; w = 5; h = 2 };
            { x; y; w = 2; h = 5 };
            { x = x + 3; y = y + 4; w = 2; h = 5 };
          |]
  | 't' ->
      fun x y ->
        Sdlrender.draw_rects renderer
          [| { x = x + 2; y; w = 2; h = 10 }; { x; y; w = 6; h = 2 } |]
  | 'u' ->
      fun x y ->
        Sdlrender.draw_rects renderer
          [|
            { x; y; w = 2; h = 10 };
            { x = x + 5; y; w = 2; h = 10 };
            { x; y = y + 8; w = 7; h = 2 };
          |]
  | 'v' ->
      fun x y ->
        Sdlrender.draw_lines renderer
          [|
            (x, y);
            (x + 2, y + 9);
            (x + 3, y + 9);
            (x + 5, y);
            (x + 4, y);
            (x + 3, y + 8);
            (x + 2, y + 8);
            (x + 1, y);
          |]
  | 'w' ->
      fun x y ->
        Sdlrender.draw_lines renderer
          [|
            (x, y);
            (x + 2, y + 9);
            (x + 3, y + 9);
            (x + 5, y);
            (x + 4, y);
            (x + 3, y + 8);
            (x + 2, y + 8);
            (x + 1, y);
            (x + 5, y);
            (x + 7, y + 9);
            (x + 8, y + 9);
            (x + 10, y);
            (x + 9, y);
            (x + 8, y + 8);
            (x + 7, y + 8);
            (x + 6, y);
          |]
  | 'y' ->
      fun x y ->
        Sdlrender.draw_rects renderer
          [|
            { x; y; w = 2; h = 4 };
            { x = x + 5; y; w = 2; h = 4 };
            { x; y = y + 3; w = 7; h = 2 };
            { x = x + 3; y = y + 3; w = 2; h = 7 };
          |]
  | 'z' ->
      fun x y ->
        Sdlrender.draw_rects renderer
          [| { x; y; w = 7; h = 2 }; { x; y = y + 8; w = 7; h = 2 } |];
        Sdlrender.draw_lines renderer
          [| (x + 6, y); (x, y + 9); (x + 1, y + 9); (x + 7, y) |]
  | ':' ->
      fun x y ->
        Sdlrender.draw_rects renderer
          [|
            { x; y = y + 2; w = 2; h = 2 };
            { x; y = y + 6; w = 2; h = 2 };
          |]
  | '#' ->
      fun x y ->
        Sdlrender.draw_rects renderer
          [|
            { x = x + 1; y; w = 2; h = 10 };
            { x = x + 4; y; w = 2; h = 10 };
            { x; y = y + 2; w = 7; h = 2 };
            { x; y = y + 5; w = 7; h = 2 };
          |]
  | '!' ->
      fun x y ->
        Sdlrender.draw_rects renderer
          [| { x; y; w = 2; h = 7 }; { x; y = y + 8; w = 2; h = 2 } |]
  | '.' ->
      fun x y ->
        Sdlrender.draw_rects renderer
          [| { x; y = y + 8; w = 2; h = 2 } |]
  | ',' ->
      fun x y ->
        Sdlrender.draw_rects renderer
          [| { x; y = y + 8; w = 2; h = 2 } |];
        Sdlrender.draw_lines renderer [| (x + 1, y + 10); (x, y + 11) |]
  | '-' ->
      fun x y ->
        Sdlrender.draw_rects renderer
          [| { x; y = y + 4; w = 5; h = 2 } |]
  | '0' ->
      fun x y ->
        Sdlrender.draw_rects renderer
          [|
            { x; y; w = 2; h = 10 };
            { x = x + 5; y; w = 2; h = 10 };
            { x; y; w = 7; h = 2 };
            { x; y = y + 8; w = 7; h = 2 };
          |];
        Sdlrender.draw_lines renderer
          [| (x + 6, y + 1); (x, y + 7); (x, y + 8); (x + 6, y + 2) |]
  | '1' ->
      fun x y ->
        Sdlrender.draw_rects renderer
          [| { x; y; w = 2; h = 2 }; { x = x + 2; y; w = 2; h = 10 } |]
  | '5' ->
      fun x y ->
        Sdlrender.draw_rects renderer
          [|
            { x; y; w = 5; h = 2 };
            { x; y = y + 4; w = 3; h = 2 };
            { x; y = y + 8; w = 3; h = 2 };
            { x; y; w = 2; h = 5 };
            { x = x + 3; y = y + 4; w = 2; h = 5 };
          |]
  | _ -> fun x y -> ()

let sample_rate = 44100
let channels = 4
let buf_len = 1024

let create_button x y w h =
  { x; y; w; h; clicked = false; color = Black }

let create_slider x y button =
  { x; y; length = 110.; notch_pos = (x, y); corr_button = button }

let get_clicked button = button.clicked
let get_x (button : button) = button.x
let get_y (button : button) = button.y
let get_w (button : button) = button.w
let get_h (button : button) = button.h
let get_x_slider (slider : slider) = slider.x
let get_y_slider (slider : slider) = slider.y
let get_notch slider = slider.notch_pos
let get_corr_button slider = slider.corr_button
let set_clicked button (v : bool) = button.clicked <- v
(* let key_pressed (e : keyboard_event) (sound : Sound.synth) = (* let
   dur = 1 in *) (* if e.keypressed then *) match e.keycode with | A ->
   (* let freq = 261. in let wave = Sound.new_wave Sound.Sine freq
   sample_rate channels buf_len in *) (* Sound.start wave; *)
   Sound.set_freq sound 261.; Printf.printf "works \n" | _ ->
   Printf.printf "" *)

let set_notch (x : int) slider =
  slider.notch_pos <- (x, snd slider.notch_pos)

let set_color button (color : color) = button.color <- color
let get_color button = match_color button.color

let match_wave wave freq =
  match wave with
  | "sine" ->
      Sound.new_wave Sound.Sine freq sample_rate channels buf_len
  | "saw" -> Sound.new_wave Sound.Saw freq sample_rate channels buf_len
  | "square" ->
      Sound.new_wave Sound.Square freq sample_rate channels buf_len
  | "triangle" ->
      Sound.new_wave Sound.Triangle freq sample_rate channels buf_len
  | _ -> Sound.new_wave Sound.Sine freq sample_rate channels buf_len

let key_pressed (e : keyboard_event) (wave : string) =
  (* let dur = 1 in *)
  (* if e.keypressed then *)
  (* let sound = *)
  let gen_wave waveform freq =
    let wave = match_wave waveform freq in
    wave
  in
  match e.keycode with
  | A -> gen_wave wave 261. (* C *)
  | W -> gen_wave wave 277. (* C# *)
  | S -> gen_wave wave 293. (* D *)
  | E -> gen_wave wave 311. (* D# *)
  | D -> gen_wave wave 329. (* E *)
  | F -> gen_wave wave 349. (* F *)
  | T -> gen_wave wave 370. (* F# *)
  | G -> gen_wave wave 392. (* G *)
  | Y -> gen_wave wave 415. (* G# *)
  | H -> gen_wave wave 440. (* A *)
  | U -> gen_wave wave 466. (* A# *)
  | J -> gen_wave wave 494. (* B *)
  | K -> gen_wave wave 523. (* C *)
  | Space -> gen_wave wave 0. (*no sound*)
  | _ -> Sound.new_wave Sound.Sine 0. sample_rate channels buf_len
