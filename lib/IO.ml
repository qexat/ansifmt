(** [print_formatted ?stylizer ?line_end ?out value ~using]
    prints [value] to [out] by formatting it with [using] -
    which provides tokenization of [value] - and [stylizer]
    which renders the resulting tokens into a pretty-printable
    string. [line_end] is appended at the end of the string.
    
    Defaults if not provided:
      - [stylizer]: the default stylizer provided by [ansifmt]
      - [line_end]: a newline ([\n])
      - [out]: the standard output ([stdout]) *)
let print_formatted
  : type t.
    ?stylizer:Formatting.Stylizer.t
    -> ?parentheses:string * string
    -> ?line_end:string
    -> ?out:out_channel
    -> t
    -> using:(module Formatting.TOKENIZABLE with type t = t)
    -> unit
  =
  fun ?stylizer ?parentheses ?(line_end = "\n") ?(out = stdout) value ~using:(module M) ->
  Printf.fprintf
    out
    "%s%s"
    (Formatting.Util.format ?stylizer ?parentheses value ~using:(module M))
    line_end
;;

let print_formatted2
  : type t.
    ?stylizer:Formatting.Stylizer.t
    -> ?line_end:string
    -> ?out:out_channel
    -> t
    -> using:(module Formatting.CONVERTIBLE with type t = t)
    -> unit
  =
  fun ?stylizer ?(line_end = "\n") ?(out = stdout) value ~using:(module M) ->
  Printf.fprintf
    out
    "%s%s"
    (Formatting.Util.format_via_element ?stylizer value ~using:(module M))
    line_end
;;
