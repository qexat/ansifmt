(** Represents an ANSI SGR escape sequence. *)
type t =
  [ `Bold
  | `Dim
  | `Italic
  | `Underline
  | `Blink
  | `Reverse
  | `Foreground of Color.t
  | `Background of Color.t
  | `Composed of t * t
  ]

(** [compose left right] combines the [left] and right
    sequences into one. *)
val compose : t -> t -> t

(** [to_attributes ansi] produces an attribute record of the
    [ansi] escape sequence. *)
val to_attributes : t -> Attributes.t

(** [to_cancelling_attributes ansi] produces an attribute
    record of the escape sequence that would cancel the effects
    of [ansi]. *)
val to_cancelling_attributes : t -> Attributes.t

(** [serialize ansi] produces a serialized representation of
    the [ansi] escape sequence. *)
val serialize : t -> string

(** [deserialize string] produces an escape sequence from a
    serialization [string]. If it fails to parse, returns
    [None]. *)
val deserialize : string -> t option

(** [show ansi] renders the [ansi] escape sequence into a
    string. *)
val show : t -> string

(** [unshow ansi] renders the ANSI escape sequence that cancels
    [ansi] into a string. *)
val unshow : t -> string

(** [wrap ansi string] wraps [string] with the rendered [ansi]
    escape sequence and its cancelling counterpart.

    For example, if [string] is ["Hello"] and [ansi] is
    [`Bold], the result will be ["\x1b\[1mHello\x1b\[22m"],
    which makes the string appear bold but not what comes
    after. *)
val wrap : t -> string -> string

(** [left & right] is the same as [compose left right]. *)
val ( & ) : t -> t -> t

module Attributes : module type of Attributes
module Color : module type of Color
