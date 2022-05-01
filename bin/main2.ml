open Graphics
open Printf
open Synth
open Sdlevent
open Sdl

let sample_rate = 1000
let channels = 4
let buf_len = 1024

let init_sound_A =
  ref (Sound.new_wave Sound.Sine 0. sample_rate channels buf_len)

let io = ref (IO.init_io channels sample_rate "plswork")
let record_button = Gui.create_button 100 200
let sine_button = Gui.create_button 175 200
let () = Gui.set_clicked sine_button true
let saw_button = Gui.create_button 250 200
let square_button = Gui.create_button 325 200
let triangle_button = Gui.create_button 400 200
let which_wave = ref "sine"

let buttons =
  [
    record_button;
    sine_button;
    saw_button;
    square_button;
    triangle_button;
  ]

let wave_buttons =
  [ sine_button; saw_button; square_button; triangle_button ]

(* creates the initial screen *)
let init_render renderer butt_lst =
  let print_x_padding x y padding lst =
    List.iteri
      (fun index charac ->
        Gui.letters renderer charac (x + (index * padding)) y)
      lst
  in

  Sdlrender.set_draw_color renderer (255, 255, 255) 255;
  Sdlrender.clear renderer;

  (* DONT TOUCH THIS *)
  Sdlrender.set_draw_color renderer (0, 0, 0) 255;
  Sdlrender.draw_rects renderer
    [|
      (* white keys *)
      { x = 100; y = 100; w = 50; h = 50 };
      { x = 164; y = 100; w = 50; h = 50 };
      { x = 228; y = 100; w = 50; h = 50 };
      { x = 292; y = 100; w = 50; h = 50 };
      { x = 354; y = 100; w = 50; h = 50 };
      { x = 418; y = 100; w = 50; h = 50 };
      { x = 482; y = 100; w = 50; h = 50 };
      { x = 546; y = 100; w = 50; h = 50 };
      (* black keys *)
      { x = 132; y = 40; w = 50; h = 50 };
      { x = 196; y = 40; w = 50; h = 50 };
      { x = 323; y = 40; w = 50; h = 50 };
      { x = 387; y = 40; w = 50; h = 50 };
      { x = 451; y = 40; w = 50; h = 50 };
      (* waveform keys *)
      { x = 100; y = 200; w = 50; h = 50 };
      { x = 175; y = 200; w = 50; h = 50 };
      { x = 250; y = 200; w = 50; h = 50 };
      { x = 325; y = 200; w = 50; h = 50 };
      { x = 400; y = 200; w = 50; h = 50 };
    |];

  (* print key on keys*)
  (* (Gui.letters renderer).o 120 120; *)

  (* "Notes: C D E F G A B C" *)
  print_x_padding 50 20 8 [ 'n'; 'o'; 't'; 'e'; 's'; ':' ];
  print_x_padding 147 20 64 [ 'c'; 'd'; ' '; 'f'; 'g'; 'a' ];
  print_x_padding 155 20 64 [ '#'; '#'; ' '; '#'; '#'; '#' ];
  print_x_padding 152 60 64 [ 'w'; 'e'; ' '; 't'; 'y'; 'u' ];
  print_x_padding 120 118 64 [ 'a'; 's'; 'd'; 'f'; 'g'; 'h'; 'j'; 'k' ];
  print_x_padding 50 160 8 [ 'n'; 'o'; 't'; 'e'; 's'; ':' ];
  print_x_padding 120 160 64 [ 'c'; 'd'; 'e'; 'f'; 'g'; 'a'; 'b'; 'c' ];

  (* function buttons *)
  print_x_padding 113 220 9 [ 'r'; 'e'; 'c' ];
  print_x_padding 190 260 8 [ 's'; 'i'; 'n' ];
  print_x_padding 260 260 8 [ 's'; 'a'; 'w' ];
  print_x_padding 340 260 9 [ 's'; 'q'; 'r' ];
  print_x_padding 410 260 8 [ 't'; 'r'; 'i' ];

  let rec init_buttons butt_lst =
    match butt_lst with
    | [] -> ()
    | button :: rest ->
        if Gui.get_clicked button then (
          Sdlrender.fill_rect renderer
            {
              x = Gui.get_x button;
              y = Gui.get_y button;
              w = 50;
              h = 50;
            };
          init_buttons rest)
        else init_buttons rest
  in
  init_buttons butt_lst

(* fills in key color when key is pressed *)
let press_note_key renderer (e : keyboard_event) =
  let keycode = e.keycode in
  let fill_at x y =
    Sdlrender.set_draw_color renderer (0, 0, 0) 255;
    Sdlrender.fill_rect renderer { x; y; w = 50; h = 50 };
    Sdlrender.render_present renderer
  in
  match keycode with
  | A -> fill_at 100 100
  | S -> fill_at 164 100
  | D -> fill_at 228 100
  | F -> fill_at 292 100
  | G -> fill_at 354 100
  | H -> fill_at 418 100
  | J -> fill_at 482 100
  | K -> fill_at 546 100
  | W -> fill_at 132 40
  | E -> fill_at 196 40
  | T -> fill_at 323 40
  | Y -> fill_at 387 40
  | U -> fill_at 451 40
  | _ -> ()

(* fills in button color when button is pressed and clears it when
   pressed again*)
let press_button x y renderer button =
  if Gui.get_clicked button = true then (
    (* button is clicked; change to unclicked*)
    Sdlrender.set_draw_color renderer (255, 255, 255) 255;
    Sdlrender.fill_rect renderer { x; y; w = 50; h = 50 };
    Sdlrender.set_draw_color renderer (0, 0, 0) 255;
    Sdlrender.draw_rect renderer { x; y; w = 50; h = 50 };
    Sdlrender.render_present renderer;
    Gui.set_clicked button false)
  else (
    (* button is not clicked; change to clicked *)
    Sdlrender.set_draw_color renderer (0, 0, 0) 255;
    Sdlrender.fill_rect renderer { x; y; w = 50; h = 50 };
    Sdlrender.render_present renderer;
    Gui.set_clicked button true)

let rec blank_waveform_buttons renderer butt_lst =
  match butt_lst with
  | [] -> ()
  | button :: rest ->
      Gui.set_clicked button false;
      Sdlrender.set_draw_color renderer (255, 255, 255) 255;
      Sdlrender.fill_rect renderer
        { x = Gui.get_x button; y = Gui.get_y button; w = 50; h = 50 };
      Sdlrender.set_draw_color renderer (0, 0, 0) 255;
      Sdlrender.draw_rect renderer
        { x = Gui.get_x button; y = Gui.get_y button; w = 50; h = 50 };
      blank_waveform_buttons renderer rest

let press_only_one_button x y renderer butt_lst =
  blank_waveform_buttons renderer butt_lst;
  Sdlrender.set_draw_color renderer (0, 0, 0) 255;
  Sdlrender.fill_rect renderer { x; y; w = 50; h = 50 };
  Sdlrender.render_present renderer

(* helper function *)
let range butx buty x y w =
  butx < x + w && butx > x && buty < y + w && buty > y

(* used to specify different functionalities for different buttons*)
let mouseclick_button e renderer =
  let x = e.mb_x in
  let y = e.mb_y in
  (* record button code *)
  if range x y 100 200 50 then (
    press_button 100 200 renderer record_button;
    if Gui.get_clicked record_button = false then IO.stop_recording !io
    else io := IO.init_io channels sample_rate "plswork"
      (* wave buttons*))
  else if range x y 175 200 50 then (
    press_only_one_button 175 200 renderer wave_buttons;
    which_wave := "sine";
    Gui.set_clicked sine_button true)
  else if range x y 250 200 50 then (
    press_only_one_button 250 200 renderer wave_buttons;
    which_wave := "saw";
    Gui.set_clicked saw_button true)
  else if range x y 325 200 50 then (
    press_only_one_button 325 200 renderer wave_buttons;
    which_wave := "square";
    Gui.set_clicked square_button true)
  else if range x y 400 200 50 then (
    press_only_one_button 400 200 renderer wave_buttons;
    which_wave := "triangle";
    Gui.set_clicked triangle_button true)
  else ()

let proc_events renderer = function
  (* | KeyDown { scancode = Sdlscancode.ESCAPE } -> print_endline
     "Goodbye"; exit 0 *)
  | KeyDown { keycode = Sdlkeycode.Escape } ->
      print_endline "Bye!";
      exit 0
  | KeyDown e ->
      press_note_key renderer e;
      let sound = Synth.Gui.key_pressed e !which_wave in
      Sound.start sound;
      if Gui.get_clicked record_button = true then
        for _ = 0 to (sample_rate / buf_len) - 1 do
          IO.record sound !io
        done;
      Sound.release sound
  (* init_sound_A := Synth.Gui.key_pressed e; *)
  (* Sound.start !init_sound_A *)
  (* | KeyUp { keycode = Sdlkeycode.A } -> Sound.release
     !init_sound_A *)
  (* Synth.Gui.key_pressed e *)
  | KeyUp e ->
      init_render renderer buttons;
      Sdlrender.render_present renderer (* Synth.Gui.key_pressed e *)
  | Mouse_Button_Down e -> mouseclick_button e renderer
  | _ -> Printf.printf ""

let () =
  Random.self_init ();
  let width, height = (680, 300) in
  Sdl.init [ `VIDEO ];
  let window, renderer =
    Sdlrender.create_window_and_renderer ~width ~height ~flags:[]
  in
  init_render renderer buttons;
  Sdlrender.render_present renderer;
  let rec aux () =
    init_render renderer buttons;
    let xy, buttons = Sdlmouse.get_state () in
    Sdlrender.draw_point renderer xy;

    Sdltimer.delay 10;
    match Sdlevent.poll_event () with
    | Some (Sdlevent.Quit _) -> Sdl.quit ()
    | Some ev ->
        proc_events renderer ev;
        aux ()
    | _ -> aux ()
  in
  aux ()