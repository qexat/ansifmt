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

(** [to_ansi styling] renders the [styling] to an ANSI escape
    sequence as a string. *)
val to_ansi : t -> string

(** [wrap ~contents styling] wraps [contents] in an ANSI escape
    sequence using [styling]. *)
val wrap : contents:string -> t -> string
