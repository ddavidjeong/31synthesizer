type state = { state : int }

let make_sound freq amp = freq *. amp
let init_state () = { state = 1 }
let kill_state = { state = 0 }
let state_state state = if state.state = 1 then true else false
