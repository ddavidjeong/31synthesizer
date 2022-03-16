type sound_state = { mutable state : int }

let init_state () = { state = 0 }
let release_state state = state.state <- 1
let dead_state state = state.state = 0