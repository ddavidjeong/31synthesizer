open Mm

val all_channels : 'a array -> ('a -> 'b) -> unit
(** [all_channels audio func] applies function [func] to all channels of
    the audio array [audio]*)

val blur : float -> Audio.t -> Audio.t
(** [blur n buf] is a new buffer with each element of buffer [buf] being
    the average of itself and the next n elements. Requires: factor must
    be greater than 0. *)

val smooth : float -> Audio.t -> Audio.t
(** [smooth factor buf] is a new buffer after applying a smoothing
    lowpass filter with factor [factor] on the buffer [buf]. Requires:
    factor must be greater than 0. *)

val adsr : float -> Audio.t -> Audio.t
val range : float -> Audio.t -> Audio.t
val envelope : int -> int -> float -> Audio.t -> Audio.t
