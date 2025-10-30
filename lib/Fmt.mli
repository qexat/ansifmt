(** A string which components can be styled with ANSI escape
    sequences. *)

include Rich_string.TYPE with type Enricher.t = Ansi.t

(** [stylize ansi rs] adds the [ansi] escape sequence to the
    rich string [rs]. Synonym of [enrich]. *)
val stylize : Ansi.t -> t -> t

(** [prune_styled rs] removes every ANSI escape sequence from
    the rich string [rs]. *)
val prune_styled : t -> t

(** [render ~with_styles rs] generates a built-in string from
    the rich string [rs] that includes ANSI escape sequences if
    [with_styles] is [true]. *)
val render : with_styles:bool -> t -> string

(** [print ?out ?ending ?with_styles rs] writes the rich string
    [rs] to the [out] channel (defaults to [stdout]),
    optionally appending [ending] (defaults to a newline),
    optionally including ANSI escape sequences (defaults to
    doing it if [out] is a TTY). *)
val print
  :  ?out:out_channel
  -> ?ending:t option
  -> ?with_styles:[< `Always | `Auto | `Never > `Auto ]
  -> t
  -> unit
