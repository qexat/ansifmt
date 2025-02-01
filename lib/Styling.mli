type t =
  { foreground : Color.t option
  ; background : Color.t option
  ; bold : bool
  ; dim : bool
  ; italic : bool
  ; underlined : bool
  }

(** [none] is the empty styling. *)
val none : t

(** [create ?foreground ?background ?bold ?dim ?italic ?underlined ()]
    creates a new style object given the provided configuration. *)
val create
  :  ?foreground:[< Color.t ]
  -> ?background:[< Color.t ]
  -> ?bold:bool
  -> ?dim:bool
  -> ?italic:bool
  -> ?underlined:bool
  -> unit
  -> t

(** [fg color] is a convenient constructor for creating composable styles.
Creates a style with the specified foreground color only. See [&] for usage examples. *)
val fg : [< Color.t ] -> t

(** [fg color] is a convenient constructor for creating composable styles.
Creates a style with the specified background color only. See [&] for usage
examples. *)
val bg : [< Color.t ] -> t

(** [bold] is a convenient constructor for creating composable styles.
Creates a bold style only. See [&] for usage examples. *)
val bold : t

(** [dim] is a convenient constructor for creating composable styles.
Creates a dim style only. See [&] for usage examples. *)
val dim : t

(** [italic] is a convenient constructor for creating composable styles.
Creates an italic style only. See [&] for usage examples. *)
val italic : t

(** [underlined] is a convenient constructor for creating composable styles.
Creates an underline style only. See [&] for usage examples. *)
val underlined : t

(** [left & right] combines two styles with the following rules:

- For colors, [right] is taken if it's present, otherwise [left]
- For boolean fields, the {b or} combination of both is applied

Usage example:

{[
  let my_style = Styling.(default_style & fg yellow & bg black & bold) in
]}
*)
val ( & ) : t -> t -> t

(** [to_ansi styling] renders the [styling] to an ANSI escape
    sequence as a string. *)
val to_ansi : t -> string

(** [wrap ~contents styling] wraps [contents] in an ANSI escape
    sequence using [styling]. *)
val wrap : contents:string -> t -> string
