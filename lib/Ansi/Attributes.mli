(** Represents the attributes of an ANSI SGR escape sequence. *)
type t

(** [create ?parameters code] produces a new attribute record
    given a SGR code and its parameters.
    If parameters are not provided, the default is an empty
    list. *)
val create : ?parameters:int list -> int -> t

(** [compose left right] combines two attribute records into
    one, with [left] being the primary one. *)
val compose : t -> t -> t

(** [serialize record] produces a serialized representation of
    the [record]. *)
val serialize : t -> string

(** [show record] renders the [record] such that it can be used
    to construct a full ANSI SGR escape sequence. *)
val show : t -> string

(** [left & right] is the same as [compose left right]. *)
val ( & ) : t -> t -> t
