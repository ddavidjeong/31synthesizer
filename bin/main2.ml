open Graphics
open Printf
open Synth
open Sdlevent
open Sdl
open Mm
open Mm_audio

let sample_rate = 44100
let channels = 4
let buf_len = 1024
let last_index = (sample_rate / buf_len) - 1

let init_sound_A =
  ref (Sound.new_wave Sound.Sine 0. sample_rate channels buf_len)

let io = ref (IO.init_io channels sample_rate "sound")

let record =
  let rec_button = Gui.create_button 500 200 50 50 in
  Gui.set_color rec_button Gui.Red;
  rec_button

let sine_button = Gui.create_button 175 200 50 50
let () = Gui.set_clicked sine_button true
let saw_button = Gui.create_button 250 200 50 50
let square_button = Gui.create_button 325 200 50 50
let triangle_button = Gui.create_button 400 200 50 50
let blur_button = Gui.create_button 190 290 40 40
let smooth_button = Gui.create_button 245 290 40 40
let envelope_button = Gui.create_button 325 285 125 50
let blur_slider = Gui.create_slider 180 380 blur_button
let smooth_slider = Gui.create_slider 180 380 smooth_button
let envelope_slider = Gui.create_slider 335 380 envelope_button
let which_wave = ref "sine"
let blur_param = ref 1.
let smooth_param = ref 1.
let envelope_param = ref 1.

let buttons =
  [
    record;
    sine_button;
    saw_button;
    square_button;
    triangle_button;
    blur_button;
    smooth_button;
    envelope_button;
  ]

let wave_buttons =
  [ sine_button; saw_button; square_button; triangle_button ]

let filter_buttons = [ blur_button; smooth_button ]

let set_color_black renderer =
  Sdlrender.set_draw_color renderer (0, 0, 0) 255

let set_color_white renderer =
  Sdlrender.set_draw_color renderer (255, 255, 255) 255

let draw_slider renderer slider =
  set_color_black renderer;
  let x = Gui.get_x_slider slider in
  let y = Gui.get_y_slider slider in
  let notch_x, notch_y = Gui.get_notch slider in
  let notch_tl_y = notch_y - 10 in
  (* draw notch *)
  Sdlrender.draw_rect renderer
    { x = notch_x; y = notch_tl_y; w = 10; h = 20 };
  Sdlrender.fill_rect renderer
    { x = notch_x; y = notch_tl_y; w = 10; h = 20 };
  (* draw slider axis *)
  Sdlrender.draw_rects renderer
    [|
      { x; y; w = 110; h = 2 };
      { x; y = y - 3; w = 2; h = 8 };
      { x = x + 27; y = y - 3; w = 2; h = 8 };
      { x = x + 54; y = y - 3; w = 2; h = 8 };
      { x = x + 81; y = y - 3; w = 2; h = 8 };
      { x = x + 108; y = y - 3; w = 2; h = 8 };
    |]

let clear_slider renderer slider =
  set_color_white renderer;
  let x = Gui.get_x_slider slider in
  let y = Gui.get_y_slider slider in
  Sdlrender.fill_rect renderer { x; y = y - 10; w = 120; h = 20 }

(* creates the initial screen *)
let init_render renderer butt_lst =
  let print_x_padding
      (x : int)
      (y : int)
      (padding : int)
      (message : string)
      (color : Gui.color) =
    (* from:
       https://stackoverflow.com/questions/10068713/string-to-list-of-char *)
    let explode s = List.init (String.length s) (String.get s) in
    let char_lst = explode message in
    List.iteri
      (fun index character ->
        Gui.letters renderer character color (x + (index * padding)) y)
      char_lst
  in
  set_color_white renderer;
  Sdlrender.clear renderer;

  (* DONT TOUCH THIS *)
  set_color_black renderer;
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
      { x = 500; y = 200; w = 50; h = 50 };
      { x = 175; y = 200; w = 50; h = 50 };
      { x = 250; y = 200; w = 50; h = 50 };
      { x = 325; y = 200; w = 50; h = 50 };
      { x = 400; y = 200; w = 50; h = 50 };
      (* filter buttons *)
      { x = 175; y = 285; w = 125; h = 50 };
      { x = 190; y = 290; w = 40; h = 40 };
      { x = 245; y = 290; w = 40; h = 40 };
      { x = 325; y = 285; w = 125; h = 50 };
    |];

  (* slider notch current position *)
  if Gui.get_clicked blur_button then draw_slider renderer blur_slider
  else if Gui.get_clicked smooth_button then
    draw_slider renderer smooth_slider
  else ();

  if Gui.get_clicked envelope_button then
    draw_slider renderer envelope_slider;

  (* "Notes: C D E F G A B C" *)
  print_x_padding 50 20 8 "notes:" Gui.Black;
  print_x_padding 147 20 64 "cd fga" Gui.Black;
  print_x_padding 155 20 64 "## ###" Gui.Black;
  print_x_padding 152 60 64 "we tyu" Gui.Black;
  print_x_padding 120 118 64 "asdfghjk" Gui.Black;
  print_x_padding 50 160 8 "notes:" Gui.Black;
  print_x_padding 120 160 64 "cdefgabc" Gui.Black;

  (* waveform buttons *)
  print_x_padding 50 260 13 "wa" Gui.Black;
  print_x_padding 73 260 9 "veform" Gui.Black;
  print_x_padding 130 260 9 "s:" Gui.Black;
  print_x_padding 510 220 9 "rec" Gui.Red;
  print_x_padding 190 260 8 "sin" Gui.Black;
  print_x_padding 260 260 8 "saw" Gui.Black;
  print_x_padding 340 260 9 "sqr" Gui.Black;
  print_x_padding 410 260 8 "tri" Gui.Black;

  (* filter labels *)
  print_x_padding 50 350 8 "filters:" Gui.Black;
  print_x_padding 195 350 8 "blur" Gui.Black;
  print_x_padding 245 350 8 "smooth" Gui.Black;
  print_x_padding 355 350 8 "envelope" Gui.Black;

  (* description *)
  (* welcome to our synthesizer! Press any of the labeled keys to play
     the corresponding note labeled either above or below the square.
     Below the notes, there are clickable settings that can change the
     waveform or add a filter over the played sound. To record your
     session, click the record button before using the synthesizer. Make
     sure to unclick record to terminate the recording. To play no
     sound, press the space bar. *)
  print_x_padding 630 20 13 "we" Gui.Black;
  print_x_padding 652 20 9 "lcom" Gui.Black;
  print_x_padding 690 20 9 "e" Gui.Black;
  print_x_padding 705 20 8 "to our synthesizer!" Gui.Black;

  print_x_padding 630 40 8 "press keys to" Gui.Black;
  print_x_padding 740 40 8 "play the corresponding" Gui.Black;
  print_x_padding 630 53 8 "note labeled above or" Gui.Black;
  print_x_padding 805 53 8 "below the square." Gui.Black;

  print_x_padding 630 73 8 "below the notes,there are clickable"
    Gui.Black;
  print_x_padding 630 86 8 "settings that can change the w" Gui.Black;
  print_x_padding 872 86 8 "aveform" Gui.Black;
  print_x_padding 630 99 8 "or apply a filter." Gui.Black;
  print_x_padding 630 119 8 "to record,click the" Gui.Black;
  print_x_padding 790 119 8 "rec" Gui.Red;
  print_x_padding 820 119 8 "button before" Gui.Black;
  print_x_padding 630 132 8 "using the sythesizer.make sure to un-"
    Gui.Black;
  print_x_padding 630 145 8 "click" Gui.Black;
  print_x_padding 680 145 8 "rec" Gui.Red;
  print_x_padding 710 145 8 "to terminate the recording." Gui.Black;

  (* Sdlrender.draw_rect renderer { x = 630; y = 20; w = 1; h = 100 };
     Sdlrender.draw_rect renderer { x = 680; y = 20; w = 1; h = 100 };
     Sdlrender.draw_rect renderer { x = 730; y = 20; w = 1; h = 100 };
     Sdlrender.draw_rect renderer { x = 780; y = 20; w = 1; h = 100 };
     Sdlrender.draw_rect renderer { x = 830; y = 20; w = 1; h = 100 };
     Sdlrender.draw_rect renderer { x = 880; y = 20; w = 1; h = 100 };
     Sdlrender.draw_rect renderer { x = 930; y = 20; w = 1; h = 100 };
     Sdlrender.draw_rect renderer { x = 980; y = 20; w = 1; h = 100
     }; *)

  (* draws buttons *)
  let rec init_buttons butt_lst =
    match butt_lst with
    | [] -> ()
    | button :: rest ->
        if Gui.get_clicked button then (
          let color = Gui.get_color button in
          Sdlrender.set_draw_color renderer color 255;
          Sdlrender.fill_rect renderer
            {
              x = Gui.get_x button;
              y = Gui.get_y button;
              w = Gui.get_w button;
              h = Gui.get_h button;
            };
          init_buttons rest)
        else init_buttons rest
  in
  init_buttons butt_lst

(* fills in key color when key is pressed *)
let press_note_key renderer (e : keyboard_event) =
  let keycode = e.keycode in
  let fill_at x y =
    set_color_black renderer;
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

let map_pos_to_param
    x
    (minx : float)
    (maxx : float)
    (min_param : float)
    (max_param : float)
    (param : float ref) =
  let new_param =
    ((x -. minx) /. (maxx -. minx) *. max_param) +. min_param
  in
  param := new_param

(* fills in button color when button is pressed and clears it when
   pressed again*)
let press_button
    (x : int)
    (y : int)
    (color : int * int * int)
    renderer
    button =
  if Gui.get_clicked button = true then (
    (* button is clicked; change to unclicked*)
    set_color_white renderer;
    Sdlrender.fill_rect renderer { x; y; w = 50; h = 50 };
    set_color_black renderer;
    Sdlrender.draw_rect renderer { x; y; w = 50; h = 50 };
    Gui.set_clicked button false;
    init_render renderer buttons)
  else (
    (* button is not clicked; change to clicked *)
    Sdlrender.set_draw_color renderer color 255;
    Sdlrender.fill_rect renderer { x; y; w = 50; h = 50 };
    Gui.set_clicked button true (* init_render renderer buttons *));
  Sdlrender.render_present renderer

let press_slider_button renderer button slider =
  let x = Gui.get_x button in
  let y = Gui.get_y button in
  let w = Gui.get_w button in
  let h = Gui.get_h button in
  if Gui.get_clicked button = true then (
    (* button is clicked; change to unclicked*)
    set_color_white renderer;
    Sdlrender.fill_rect renderer { x; y; w; h };
    set_color_black renderer;
    Sdlrender.draw_rect renderer { x; y; w; h };
    clear_slider renderer slider;
    Gui.set_clicked button false)
  else (
    (* button is not clicked; change to clicked *)
    set_color_black renderer;
    Sdlrender.fill_rect renderer { x; y; w; h };
    draw_slider renderer slider;
    Gui.set_clicked button true);
  Sdlrender.render_present renderer

let rec blank_buttons renderer butt_lst (w : int) (h : int) =
  match butt_lst with
  | [] -> ()
  | button :: rest ->
      Gui.set_clicked button false;
      set_color_white renderer;
      Sdlrender.fill_rect renderer
        { x = Gui.get_x button; y = Gui.get_y button; w; h };
      set_color_black renderer;
      Sdlrender.draw_rect renderer
        { x = Gui.get_x button; y = Gui.get_y button; w; h };
      blank_buttons renderer rest w h

(* helper function *)
(* primitive version of range manual input x and y of button *)
let range_prim butx buty (x : int) (y : int) (w : int) (h : int) =
  butx < x + w && butx > x && buty < y + h && buty > y

let range (x : int) (y : int) (w : int) (h : int) button =
  let butx = Gui.get_x button in
  let buty = Gui.get_y button in
  x < butx + w && x > butx && y < buty + h && y > buty

let press_only_one_button
    (x : int)
    (y : int)
    (w : int)
    (h : int)
    renderer
    butt_lst =
  blank_buttons renderer butt_lst w h;
  set_color_black renderer;
  Sdlrender.fill_rect renderer { x; y; w; h };
  Sdlrender.render_present renderer

let press_only_one_filter
    (x : int)
    (y : int)
    (w : int)
    (h : int)
    renderer
    butt_lst
    filter_slider =
  let all_unclicked =
    List.fold_left
      (fun acc b -> (not (Gui.get_clicked b)) && acc)
      true butt_lst
  in
  let corr_button = Gui.get_corr_button filter_slider in
  if all_unclicked || not (Gui.get_clicked corr_button) then (
    blank_buttons renderer butt_lst w h;
    clear_slider renderer filter_slider;
    set_color_black renderer;
    Sdlrender.fill_rect renderer { x; y; w; h };
    draw_slider renderer filter_slider;
    Gui.set_clicked corr_button true)
  else (
    blank_buttons renderer butt_lst w h;
    clear_slider renderer filter_slider);
  Sdlrender.render_present renderer

let constrain_notch (x : int) (minx : int) (maxx : int) =
  if x < minx then minx else if x > maxx then maxx else x

(* used to specify different functionalities for different buttons*)
let mouseclick_button (e : mouse_button_event) renderer =
  let x = e.mb_x in
  let y = e.mb_y in
  let update_slider slider param =
    let minx = Gui.get_x_slider slider in
    let new_x = constrain_notch x minx (minx + 100) in
    clear_slider renderer slider;
    Gui.set_notch new_x slider;
    draw_slider renderer slider;
    let float_x = float_of_int x in
    let float_minx = float_of_int minx in
    map_pos_to_param float_x float_minx (float_minx +. 100.) 1. 9. param;
    Sdlrender.render_present renderer
  in
  let click_wave_button button waveform =
    press_only_one_button (Gui.get_x button) (Gui.get_y button) 50 50
      renderer wave_buttons;
    which_wave := waveform;
    Gui.set_clicked button true
  in
  (* record button code *)
  if range x y 50 50 record then (
    press_button 500 200 (252, 3, 3) renderer record;
    if Gui.get_clicked record = false then IO.stop_recording !io
    else io := IO.init_io channels sample_rate "sound" (* wave buttons*))
  else if range x y 50 50 sine_button then
    click_wave_button sine_button "sine"
  else if range x y 50 50 saw_button then
    click_wave_button saw_button "saw"
  else if range x y 50 50 square_button then
    click_wave_button square_button "square"
  else if range x y 50 50 triangle_button then
    click_wave_button triangle_button "triangle" (* blur *)
  else if range x y 40 40 blur_button then
    press_only_one_filter 190 290 40 40 renderer filter_buttons
      blur_slider (* smooth *)
  else if range x y 40 40 smooth_button then
    press_only_one_filter 245 290 40 40 renderer filter_buttons
      smooth_slider (* envelope *)
  else if range_prim x y 325 285 125 50 then
    press_slider_button renderer envelope_button envelope_slider
  else if
    (* blur slider *)
    range_prim x y 175 370 120 20 && Gui.get_clicked blur_button
  then update_slider blur_slider blur_param
  else if
    (* smooth slider *)
    range_prim x y 175 370 120 20 && Gui.get_clicked smooth_button
  then update_slider smooth_slider smooth_param
  else if
    (* envelope slider *)
    range_prim x y 320 370 120 20 && Gui.get_clicked envelope_button
  then update_slider envelope_slider envelope_param
  else ()

(* blur and smooth filters *)
let writebuff_and_play sound filter param =
  for i = 0 to (sample_rate / buf_len) - 1 do
    Sound.start_generator sound;
    let buf = Sound.get_buf sound in
    let new_buf = filter param buf in
    Sound.set_buf sound new_buf;
    Sound.start sound;
    if Gui.get_clicked record then IO.record sound !io
  done

(* envelope filters *)
(* let write_and_play_envelope sound = for i = 0 to last_index do
   Sound.start_generator sound; let buf = Sound.get_buf sound in let
   new_buf = Filters.envelope i last_index !envelope_param buf in
   Sound.set_buf sound new_buf; Sound.start sound; if Gui.get_clicked
   record then IO.record sound !io done *)

(* envelope filter and either blur or smooth filter *)
(* let writebuff_and_play2 sound filter param = for i = 0 to last_index
   do Sound.start_generator sound; let buf = Sound.get_buf sound in let
   new_buf = Filters.envelope i last_index !envelope_param buf in let
   new_buf = filter param new_buf in Sound.set_buf sound new_buf;
   Sound.start sound; if Gui.get_clicked record then IO.record sound !io
   done *)

let writebuff_and_play3 ?(filter = fun x y -> y) sound param =
  for i = 0 to last_index do
    Sound.start_generator sound;
    let buf = Sound.get_buf sound in
    let new_buf = Filters.envelope i last_index !envelope_param buf in
    let new_buf = filter param new_buf in
    Sound.set_buf sound new_buf;
    Sound.start sound;
    if Gui.get_clicked record then IO.record sound !io
  done

let proc_events renderer = function
  | KeyDown { keycode = Sdlkeycode.Escape } ->
      print_endline "Bye!!";
      exit 0
  | KeyDown e ->
      press_note_key renderer e;
      let sound = Gui.key_pressed e !which_wave in
      if Gui.get_clicked envelope_button && Gui.get_clicked blur_button
      then writebuff_and_play3 ~filter:Filters.blur sound !blur_param
      else if
        Gui.get_clicked envelope_button && Gui.get_clicked smooth_button
      then
        writebuff_and_play3 ~filter:Filters.smooth sound !smooth_param
      else if Gui.get_clicked envelope_button then
        writebuff_and_play3 sound !envelope_param
      else if Gui.get_clicked blur_button then
        writebuff_and_play sound Filters.blur !blur_param
      else if Gui.get_clicked smooth_button then
        writebuff_and_play sound Filters.smooth !smooth_param
      else
        for i = 0 to (sample_rate / buf_len * 1) - 1 do
          Sound.start_generator sound;
          Sound.start sound;
          if Gui.get_clicked record then IO.record sound !io
        done;
      Sound.release sound
  | KeyUp e ->
      init_render renderer buttons;
      Sdlrender.render_present renderer (* Synth.Gui.key_pressed e *)
  | Mouse_Button_Down e -> mouseclick_button e renderer
  | _ -> Printf.printf ""

let () =
  Random.self_init ();
  let width, height = (970, 400) in
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