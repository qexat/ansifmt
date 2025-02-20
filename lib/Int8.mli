(** Represents an 8-bit integer. *)

type t = private int

(** [to_int value] converts [value] to a built-in integer. *)
val to_int : t -> int

(** [of_int value] converts [value] to an 8-bit integer.
    Returns [None] if [value] does not fit in 8 bits. *)
val of_int : int -> t option

(** [of_int_exn value] converts [value] to a 8-bit integer.
    Raises a [Failure] exception if [value] does not fit in 8
    bits. *)
val of_int_exn : int -> t
