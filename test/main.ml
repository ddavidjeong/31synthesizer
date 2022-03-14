open OUnit2
open Enigma

(** [index_test name input expected_output] constructs an OUnit test
    named [name] that asserts the quality of [expected_output] with
    [index input]. *)
let index_test (name : string) (input : char) (expected_output : int) :
    test =
  name >:: fun _ ->
  assert_equal expected_output (index input) ~printer:string_of_int

let index_tests =
  [ index_test "index of A is 0" 'A' 0 (* TODO: add your tests here *) ]

let tests = "test suite for A1" >::: List.flatten [ index_tests ]
let _ = run_test_tt_main tests
