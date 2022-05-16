open Mm

val all_channels : 'a array -> ('a -> 'b) -> unit
val blur : float -> Audio.t -> Audio.t
val smooth : float -> Audio.t -> Audio.t
val envelope : int -> int -> float -> Audio.t -> Audio.t
val adsr : float -> Audio.t -> Audio.t
val range : float -> Audio.t -> Audio.t

