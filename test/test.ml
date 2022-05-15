open Synth
open OUnit2
open Synth.Filters
open Mm

let array_printer arr =
  let new_arr = arr.(0) in
  "[|"
  ^ Array.fold_left
      (fun acc elem -> acc ^ ", " ^ string_of_float elem)
      "" new_arr
  ^ "|]"

let smooth_test
    (name : string)
    (expected : float array array)
    (smoothing : float)
    (input : Audio.t) : test =
  name >:: fun _ ->
  assert_equal expected
    (smooth smoothing input |> Audio.to_array)
    ~printer:array_printer

let blur_test
    (name : string)
    (expected : float array array)
    (blur_factor : float)
    (input : Audio.t) : test =
  name >:: fun _ ->
  assert_equal expected
    (blur blur_factor input |> Audio.to_array)
    ~printer:array_printer

let get_waveform_test
    (name : string)
    (sound : Sound.synth)
    (expect_output : Sound.wave) : test =
  name >:: fun _ ->
  assert_equal expect_output (Sound.get_waveform sound)

let get_frequency_test
    (name : string)
    (sound : Sound.synth)
    (expect_output : float) : test =
  name >:: fun _ -> assert_equal expect_output (Sound.get_freq sound)

let set_frequency_test
    (name : string)
    (freq : float)
    (sound : Sound.synth)
    (expect_output : float) : test =
  name >:: fun _ ->
  assert_equal expect_output
    (Sound.set_freq sound freq;
     Sound.get_freq sound)

let get_sr_test
    (name : string)
    (sound : Sound.synth)
    (expect_output : int) : test =
  name >:: fun _ -> assert_equal expect_output (Sound.get_sr sound)

let is_playing_test
    (name : string)
    (sound : Sound.synth)
    (expect_output : bool) : test =
  name >:: fun _ -> assert_equal expect_output (Sound.is_playing sound)

let get_waveform_test
    (name : string)
    (sound : Sound.synth)
    (expect_output : Sound.wave) : test =
  name >:: fun _ ->
  assert_equal expect_output (Sound.get_waveform sound)

let get_clicked_button_test
    (name : string)
    (button : Gui.button)
    (expect_output : bool) : test =
  name >:: fun _ -> assert_equal expect_output (Gui.get_clicked button)

let get_x_button_test
    (name : string)
    (button : Gui.button)
    (expect_output : int) : test =
  name >:: fun _ -> assert_equal expect_output (Gui.get_x button)

let get_y_button_test
    (name : string)
    (button : Gui.button)
    (expect_output : int) : test =
  name >:: fun _ -> assert_equal expect_output (Gui.get_y button)

let get_w_button_test
    (name : string)
    (button : Gui.button)
    (expect_output : int) : test =
  name >:: fun _ -> assert_equal expect_output (Gui.get_w button)

let get_h_button_test
    (name : string)
    (button : Gui.button)
    (expect_output : int) : test =
  name >:: fun _ -> assert_equal expect_output (Gui.get_h button)

let get_color_button_test
    (name : string)
    (button : Gui.button)
    (expect_output : int * int * int) : test =
  name >:: fun _ -> assert_equal expect_output (Gui.get_color button)

let get_x_slider_test
    (name : string)
    (slider : Gui.slider)
    (expect_output : int) : test =
  name >:: fun _ -> assert_equal expect_output (Gui.get_x_slider slider)

let get_y_slider_test
    (name : string)
    (slider : Gui.slider)
    (expect_output : int) : test =
  name >:: fun _ -> assert_equal expect_output (Gui.get_y_slider slider)

let get_notch_test
    (name : string)
    (slider : Gui.slider)
    (expect_output : int * int) : test =
  name >:: fun _ -> assert_equal expect_output (Gui.get_notch slider)

let get_corr_button_test
    (name : string)
    (slider : Gui.slider)
    (expect_output : Gui.button) : test =
  name >:: fun _ ->
  assert_equal expect_output (Gui.get_corr_button slider)

let set_buf_test
    (name : string)
    (buf : Audio.t)
    (sound : Sound.synth)
    (expect_output : float array array) : test =
  name >:: fun _ ->
  assert_equal expect_output
    (Sound.set_buf sound buf;
     Sound.get_buf sound |> Audio.to_array)

let get_filename_test
    (name : string)
    (io : IO.io_t)
    (expect_output : string) : test =
  name >:: fun _ -> assert_equal expect_output (IO.get_filename io)

let sound1 = Sound.new_wave Sine 0. 1000 4 1024
let sound2 = Sound.new_wave Sine 100. 1000 2 1024
let sound3 = Sound.new_wave Square 100. 44100 2 1024
let sound4 = Sound.new_wave Saw 100. 44100 2 1024
let sound5 = Sound.new_wave Triangle 100. 44100 2 1024
let button1 = Gui.create_button 1 2 3 4

let butt_mutateclick =
  let x = Gui.create_button 1 2 3 4 in
  Gui.set_clicked x true;
  x

let butt_mutatecolor =
  let x = Gui.create_button 1 2 3 4 in
  Gui.set_color x Gui.Red;
  x

let slider1 = Gui.create_slider 5 6 button1

let mutated_slider =
  let x = Gui.create_slider 5 6 button1 in
  Gui.set_notch 10 x;
  x

let io1 = IO.init_io 4 44100 "filename"
let io2 = IO.init_io 4 44100 ""

(* let arr = [| 1.0; 2.0; 3.0 |] *)
let buf1 =
  let a = [| [| 1.0 |] |] in
  Audio.of_array a

let buf2 =
  let a = [| [| 0. |] |] in
  Audio.of_array a

let buf3 =
  let a = [| [| 1.0; 2.0; 3.0 |] |] in
  Audio.of_array a

let buf4 =
  let a = [| [| 1.0 |]; [| 1.0 |] |] in
  Audio.of_array a

let filter_tests =
  [
    smooth_test "smooth test with no-change smoothing"
      [| [| 1.0; 2.0; 3.0 |] |]
      1.0 buf3;
    smooth_test "smooth\n test of 2.0 smoothing"
      [| [| 1.0 |] |]
      2.0 buf1;
    smooth_test "smooth test of negative smoothing value"
      [| [| 1.0 |] |]
      (-1.0) buf1;
    blur_test "blur test with no-change blurring"
      [| [| 1.0; 2.0; 3.0 |] |]
      3. buf3;
    blur_test "blur test of 1.0 blurring"
      [| [| 3.0; 5.0; 3.0 |] |]
      1. buf3;
  ]

let sound_tests =
  [
    get_waveform_test "get sine wave" sound2 Sound.Sine;
    get_waveform_test "get square wave" sound3 Sound.Square;
    get_waveform_test "get saw wave" sound4 Sound.Saw;
    get_waveform_test "get triangle wave" sound5 Sound.Triangle;
    get_frequency_test "freq 0 for sound 1" sound1 0.;
    get_frequency_test "freq 100 for sound 3" sound3 100.;
    set_frequency_test "freq 0 for sound 2" 0. sound2 0.;
    set_frequency_test "freq 10000 for sound 2" 10000. sound2 10000.;
    get_sr_test "sr 1000 for sound2" sound2 1000;
    is_playing_test "sound is not playing" sound2 false;
    set_buf_test "set buf to an array of [|[|1.0|]|]" buf1 sound5
      [| [| 1.0 |] |];
    set_buf_test "set buf to an array of [|[||]|]" buf2 sound5
      [| [||] |];
  ]

let gui_tests =
  [
    get_clicked_button_test "get clicked of button1" button1 false;
    get_x_button_test "get x of button1" button1 1;
    get_y_button_test "get y of button1" button1 2;
    get_w_button_test "get w of button1" button1 3;
    get_h_button_test "get h of button1" button1 4;
    get_color_button_test "get color of button1" button1 (0, 0, 0);
    get_clicked_button_test
      "get clicked of butt_mutateclick; false -> true" butt_mutateclick
      true;
    get_color_button_test "get color of butt_mutatecolor; Black -> Red"
      butt_mutatecolor (252, 3, 3);
    get_x_slider_test "get x of slider1" slider1 5;
    get_y_slider_test "get y of slider1" slider1 6;
    get_notch_test "get notch of slider1" slider1 (5, 6);
    get_corr_button_test "get button of slider1" slider1 button1;
    get_notch_test "get notch of mutated_slider; x -> 10" mutated_slider
      (10, 6);
  ]

let io_tests =
  [
    get_filename_test "filename of io1 is 'filename'" io1 "filename";
    get_filename_test "filename of io1 is ''" io2 "";
  ]

let suite =
  "test suite"
  >::: List.flatten [ sound_tests; gui_tests; io_tests; filter_tests ]

let () = run_test_tt_main suite
