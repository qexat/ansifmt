(** Represents a formatting element. *)
type t

(** Represents a pair of characters (e.g. parentheses). *)
and pair =
  | Parentheses
  | Brackets
  | Braces
  | Custom_pair of string * string

(** [singleton token] creates a singleton element of [token]. *)
val singleton : Token.t -> t

(** [blob tokens] groups [tokens] into an element. *)
val blob : Token.t list -> t

(** [cluster elements] groups [elements] together. *)
val cluster : t list -> t

(** [indented ~indent_count element] produces a new element
  that will be indented [indent_count] times when formatting
  it. Returns [None] if [indent_count] is negative. *)
val indented : indent_count:int -> t -> t option

(** [indented_exn ~indent_count element] produces a new
  element that will be indented [indent_count] times when
  formatting it. Raises [Invalid_arg] if [indent_count] is
  negative. *)
val indented_exn : indent_count:int -> t -> t

(** [parenthesized ~pair ?condition element] produces a new
  element that will be surrounded by a [pair] when formatting
  it. A [condition] can be optionally provided that will leave
  the element as-is if it is not met. *)
val parenthesized : ?pair:pair -> ?condition:(t -> bool) -> t -> t

(** [intercalated ~separating elements] produces a new element
  where the [separating] list of tokens is intercalated
  between the [elements]. *)
val intercalated : separating:Token.t list -> t -> t

(** [sequence ~pair elements] produces a new element where the
  [elements] are separated by a comma (and a space) and
  surrounded by [pair]. *)
val sequence : pair:pair -> t -> t

(** [lines elements] produces a new element that intersperses
  a line break between [elements] when formatting it,
  WITHOUT a trailing newline. *)
val lines : t -> t

(** [format ~stylizer element] renders the [element] into a
  string using the [stylizer]. *)
val format : ?stylizer:Stylizer.t -> t -> string
