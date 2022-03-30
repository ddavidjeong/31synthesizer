open Graphics
open Printf
open Synth

(* Displays mouse position and keys pressed in the graphics window, and
   exits if q is pressed. *)

let print_at x y message =
  moveto x y;
  draw_string message

let rec loop () =
  print_at 45 525 "Note: C";
  print_at 165 525 "Note: D";
  print_at 285 525 "Note: E";
  print_at 405 525 "Note: F";
  print_at 525 525 "Note: G";
  draw_rect 20 480 100 100;
  draw_rect 140 480 100 100;
  draw_rect 260 480 100 100;
  draw_rect 380 480 100 100;
  draw_rect 500 480 100 100;

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
  print_at 45 525 "Note: C";
  print_at 165 525 "Note: D";
  print_at 285 525 "Note: E";
  print_at 405 525 "Note: F";
  print_at 525 525 "Note: G";
  draw_rect 20 480 100 100;
  draw_rect 140 480 100 100;
  draw_rect 260 480 100 100;
  draw_rect 380 480 100 100;
  draw_rect 500 480 100 100;

  set_color black;
  if e.key == 'q' then fill_rect 20 480 100 100;
  if e.key == 'w' then fill_rect 140 480 100 100;
  if e.key == 'e' then fill_rect 260 480 100 100;
  if e.key == 'r' then fill_rect 380 480 100 100;
  if e.key == 't' then fill_rect 500 480 100 100;
  Synth__Gui.key_pressed e;
  let test = sprintf "test: %B" e.button in
  print_at 200 150 test;

  draw_rect 20 20 100 100;

  set_color white;
  if e.key == 'q' then fill_rect 20 480 100 100;
  if e.key == 'w' then fill_rect 140 480 100 100;
  if e.key == 'e' then fill_rect 260 480 100 100;
  if e.key == 'r' then fill_rect 380 480 100 100;
  if e.key == 't' then fill_rect 500 480 100 100;

  if e.key <> 'p' then loop () else ()

let () =
  Graphics.open_graph " 800x600";
  loop ();
  close_graph ()
