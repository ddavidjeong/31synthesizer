open Mm

type sound_state = { mutable state : int }

let init_state () = { state = 0 }
let release_state state = state.state <- 1
let dead_state state = state.state = 0

let create_ao_state channels sample_rate =
  new Mm_ao.writer channels sample_rate

let create_buf channels buf_len = Audio.create channels buf_len