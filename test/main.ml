open OUnit2
open Synth
open Synth.Filters

let gui_test (name : string) func (input : char) (expected_output : int)
    : test =
  name >:: fun _ ->
  (* the [printer] tells OUnit how to convert the output to a string *)
  assert_equal expected_output (func input) ~printer:string_of_int

let generator_test
    (name : string)
    func
    (input : char)
    (expected_output : int) : test =
  name >:: fun _ ->
  (* the [printer] tells OUnit how to convert the output to a string *)
  assert_equal expected_output (func input) ~printer:string_of_int

let filter_test
    (name : string)
    func
    (input : char)
    (expected_output : int) : test =
  name >:: fun _ ->
  (* the [printer] tells OUnit how to convert the output to a string *)
  assert_equal expected_output (func input) ~printer:string_of_int

let gui_tests = []
let generator_tests = []
let filter_tests = []

let tests =
  "test suite for Synth"
  >::: List.flatten [ gui_tests; generator_tests; filter_tests ]
