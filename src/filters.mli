open Mm

val all_channels : 'a array -> ('a -> 'b) -> unit
(** [all_channels audio func] applies function [func] to all channels of
    the audio array [audio]*)

val iden : float -> Audio.t -> Audio.t 
(** [iden factor buf] is an identity filter than does not change the input 
    buffer. [factor] is a placeholder and is ignored. *)

val blur : float -> Audio.t -> Audio.t
(** [blur factor buf] is a new buffer with each element of buffer [buf] being
    the average of itself and the next n elements. Requires: factor must
    be greater than 0. *)

val smooth : float -> Audio.t -> Audio.t
(** [smooth factor buf] is a new buffer after applying a smoothing
    lowpass filter with factor [factor] on the buffer [buf]. Requires:
    factor must be greater than 0. *)

val adsr : float -> Audio.t -> Audio.t
(** [adsr factor buf] is a new buffer after applying our adsr cycle filter 
    with inten [inten] on the buffer [buf] Requires:
    inten must be greater than 0.0 and less than 10.0 *)

val range : float -> Audio.t -> Audio.t
(** [range factor buf] is a new buffer with the ratio being factored to a ratio 
    with inten [inten] on the buffer [buf] Requires:
    inten must be greater than 0.0 and less than 10.0 *)

val envelope : int -> int -> float -> Audio.t -> Audio.t
(** [envelope factor index last_index buf] is a new buffer after applying an
    envelope to [buf] with a rate of oscillation related to the sample rate of [buf] by 
    [factor]. *)


