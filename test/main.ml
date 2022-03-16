open OUnit2
open Synth
open Mm_audio

let sine_sound =
  new Audio.Generator.of_mono
    (new Audio.Mono.Generator.sine 44000 440.0)
