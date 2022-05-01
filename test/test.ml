open Synth
open OUnit2

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

let sound1 = Sound.new_wave Sine 0. 1000 4 1024
let sound2 = Sound.new_wave Sine 100. 1000 2 1024
let sound3 = Sound.new_wave Square 100. 44100 2 1024
let sound4 = Sound.new_wave Saw 100. 44100 2 1024
let sound5 = Sound.new_wave Triangle 100. 44100 2 1024

let sound_tests =
  [
    get_waveform_test "get sine wave" sound2 Sound.Sine;
    get_waveform_test "get square wave" sound3 Sound.Square;
    get_waveform_test "get saw wave" sound4 Sound.Saw;
    get_waveform_test "get triangle wave" sound5 Sound.Triangle;
    get_frequency_test "freq 0 for sound 1" sound1 0.;
    get_frequency_test "freq 100 for sound 3" sound3 100.;
    get_sr_test "sr 44100 for sound2" sound2 44100;
    is_playing_test "sound is not playing" sound2 false;
  ]

let suite = "test suite for A2" >::: List.flatten [ sound_tests ]
let _ = run_test_tt_main suite
