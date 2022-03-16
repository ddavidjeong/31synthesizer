type sound_state

(* [init_state] sets up an initial sound state *)
val init_state : unit -> sound_state

(* [release_state state] set [state] to the release state *)
val release_state : sound_state -> unit

(* [is_dead state] returns true if the state is in the dead state *)
val dead_state : sound_state -> bool
