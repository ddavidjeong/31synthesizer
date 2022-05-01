open Sdlevent
open Printf
open Sdl
(* open Sound *)

type button = {
  x : int;
  y : int;
  mutable clicked : bool;
}

let letters renderer char =
  match char with
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
  | _ -> fun x y -> ()

let sample_rate = 1000
let channels = 4
let buf_len = 1024
let create_button x y = { x; y; clicked = false }
let get_clicked button = button.clicked
let get_x button = button.x
let get_y button = button.y
let set_clicked button v = button.clicked <- v
(* let key_pressed (e : keyboard_event) (sound : Sound.synth) = (* let
   dur = 1 in *) (* if e.keypressed then *) match e.keycode with | A ->
   (* let freq = 261. in let wave = Sound.new_wave Sound.Sine freq
   sample_rate channels buf_len in *) (* Sound.start wave; *)
   Sound.set_freq sound 261.; Printf.printf "works \n" | _ ->
   Printf.printf "" *)

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
  match e.keycode with
  | A ->
      (* C *)
      let freq = 261. in
      let wave = match_wave wave freq in
      (* Sound.start wave; *)
      wave
  | W ->
      (* C# *)
      let freq = 277. in
      let wave = match_wave wave freq in
      wave
  | S ->
      (* D *)
      let freq = 293. in
      let wave = match_wave wave freq in
      (* Sound.start wave; *) wave
  | E ->
      (* D# *)
      let freq = 311. in
      let wave = match_wave wave freq in
      wave
  | D ->
      (* E *)
      let freq = 329. in
      let wave = match_wave wave freq in
      (* Sound.start wave; *) wave
  | F ->
      (* F *)
      let freq = 349. in
      let wave = match_wave wave freq in
      (* Sound.start wave; *) wave
  | T ->
      (* F# *)
      let freq = 370. in
      let wave = match_wave wave freq in
      (* Sound.start wave; *) wave
  | G ->
      (* G *)
      let freq = 392. in
      let wave = match_wave wave freq in
      (* Sound.start wave; *) wave
  | Y ->
      (* G# *)
      let freq = 415. in
      let wave = match_wave wave freq in
      (* Sound.start wave; *) wave
  | H ->
      (* A *)
      let freq = 440. in
      let wave = match_wave wave freq in
      (* Sound.start wave; *) wave
  | U ->
      (* A# *)
      let freq = 466. in
      let wave = match_wave wave freq in
      (* Sound.start wave; *) wave
  | J ->
      (* B *)
      let freq = 494. in
      let wave = match_wave wave freq in
      (* Sound.start wave; *) wave
  | K ->
      (* C *)
      let freq = 523. in
      let wave = match_wave wave freq in
      (* Sound.start wave; *) wave
  | _ -> Sound.new_wave Sound.Sine 0. sample_rate channels buf_len
(* in sound *)
(* if e.ke_state = Pressed then Sound.start sound *)
(* else Sound.release sound *)