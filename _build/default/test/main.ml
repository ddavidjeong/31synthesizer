open OUnit2
open Synth

let index_tests = []
let tests = "test suite" >::: List.flatten []
let _ = run_test_tt_main tests
