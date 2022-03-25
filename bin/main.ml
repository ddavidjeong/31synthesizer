open Graphics
open Printf

(* Displays mouse position and keys pressed in the graphics window, and
   exits if q is pressed. *)
let rec loop () =
  let e = wait_next_event [ Mouse_motion; Key_pressed ] in

  let mouse_description =
    sprintf "Mouse position: %d,%d" e.mouse_x e.mouse_y
  in
  let key_description =
    if e.keypressed then sprintf "Key %c was pressed" e.key else ""
  in

  clear_graph ();
  moveto 100 100;
  draw_string key_description;
  moveto 0 0;
  draw_string mouse_description;
  draw_rect 20 20 100 100;
  draw_rect 20 140 100 100;
  draw_rect 20 260 100 100;
  draw_rect 20 380 100 100;

  if e.key <> 'q' then loop () else ()

let () =
  Graphics.open_graph " 800x800";
  loop ();
  close_graph ()
