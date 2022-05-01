open OUnit2

let gui_tests = []

let generator_tests = []

let filter_tests = []

let tests =
  "test suite for Synth"
  >::: List.flatten
         [
           gui_tests;
           generator_tests;
           filter_tests
         ]

let _ = run_test_tt_main tests