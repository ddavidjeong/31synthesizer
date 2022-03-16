open OUnit2
open Synth
open Mm_audio


let () =  
let buf = Audio.create 2 1000 in
new Audio.Generator.of_mono
    (new Audio.Mono.Generator.sine sample_rate (440.0 *. freq))
in 
for _ = 0 to 10 do 
  sine#fill buf 0 1024;
done;