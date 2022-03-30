open Graphics
open Printf
open Synth

(* Displays mouse position and keys pressed in the graphics window, and
   exits if q is pressed. *)

let sounds = []

let print_at x y message =
  moveto x y;
  draw_string message

let go = ref true

let rec loop () =
  print_at 45 325 "Note: C";
  print_at 165 325 "Note: D";
  print_at 285 325 "Note: E";
  print_at 405 325 "Note: F";
  print_at 525 325 "Note: G";
  print_at 645 325 "Note: A";
  print_at 765 325 "Note: B";
  print_at 885 325 "Note: C";
  draw_rect 20 280 100 100;
  draw_rect 140 280 100 100;
  draw_rect 260 280 100 100;
  draw_rect 380 280 100 100;
  draw_rect 500 280 100 100;
  draw_rect 620 280 100 100;
  draw_rect 740 280 100 100;
  draw_rect 860 280 100 100;

  let e = wait_next_event [ Key_pressed ] in
  let key_description =
    if e.keypressed then sprintf "Key %c was pressed" e.key else ""
  in

  let mouse_description =
    sprintf "Mouse position: %d,%d" e.mouse_x e.mouse_y
  in
  print_at 300 150 mouse_description;
  if e.keypressed then print_at 100 100 key_description;

  clear_graph ();
  print_at 45 325 "Note: C";
  print_at 165 325 "Note: D";
  print_at 285 325 "Note: E";
  print_at 405 325 "Note: F";
  print_at 525 325 "Note: G";
  print_at 645 325 "Note: A";
  print_at 765 325 "Note: B";
  print_at 885 325 "Note: C";
  draw_rect 20 280 100 100;
  draw_rect 140 280 100 100;
  draw_rect 260 280 100 100;
  draw_rect 380 280 100 100;
  draw_rect 500 280 100 100;
  draw_rect 620 280 100 100;
  draw_rect 740 280 100 100;
  draw_rect 860 280 100 100;

  set_color black;
  if e.key == 'a' then fill_rect 20 280 100 100;
  if e.key == 's' then fill_rect 140 280 100 100;
  if e.key == 'd' then fill_rect 260 280 100 100;
  if e.key == 'f' then fill_rect 380 280 100 100;
  if e.key == 'g' then fill_rect 500 280 100 100;
  if e.key == 'h' then fill_rect 620 280 100 100;
  if e.key == 'j' then fill_rect 740 280 100 100;
  if e.key == 'k' then fill_rect 860 280 100 100;
  Synth__Gui.key_pressed e;
  let test = sprintf "test: %B" e.button in
  print_at 200 150 test;

  draw_rect 20 20 100 100;
  if e.key <> 'p' then loop () else ()

let () =
  Graphics.open_graph " 1000x600";
  loop ();
  close_graph ()
