(** A string which components can be stylized. *)
type t =
  | Composed of t * t
  | Raw of string
  | Stylized of t * Ansi.t

(** [compose left right] composes [left] and [right] together. *)
val compose : t -> t -> t

(** [raw string] lifts a simple built-in string to the
    stylizable realm. *)
val raw : string -> t

(** [stylize fmt ansi] produces a new stylized string by
    attaching the [ansi] styling to the [fmt] stylized string. *)
val stylize : t -> Ansi.t -> t

(** [join ~on fmts] intercalates [on] between each string of
    [fmts] and concatenates the result. *)
val join : on:t -> t list -> t

(** [render ~with_styling fmt] renders [fmt] into a built-in,
    non-manipulable string. *)
val render : with_styling:bool -> t -> string

(** [print ?out ?ending ?with_styling fmt] prints [fmt] in
    [out] (stdout by default) ending with [ending]
    (["\n" by default]).

    Styling displaying is controlled by [with_styling]:
      - [`Always] always prints styling
      - [`Never] never prints styling
      - [`Auto] prints styling if [out] is a TTY *)
val print
  :  ?out:out_channel
  -> ?ending:string option
  -> ?with_styling:[ `Auto | `Always | `Never ]
  -> t
  -> unit

(** [serialize fmt] produces a serialized representation of
    [fmt]. *)
val serialize : t -> string

(** [show fmt] produces a pretty-printable string from [fmt]. *)
val show : t -> t

(** [left ++ right] concatenates [left] and [right] together. *)
val ( ++ ) : t -> t -> t
