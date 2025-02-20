open Formatting

(** [print_formatted ?stylizer ?line_end ?out value ~using]
    prints [value] to [out] by formatting it with [using] --
    which provides conversion of [value] to a formatting element
    -- and [stylizer] which renders the resulting element into
    a pretty-printable string. [line_end] is appended at the end
    of the string.

    This function is meant to have an interface resembling
    Python's [print] function.
    
    Defaults if not provided:
      - [stylizer]: the default stylizer provided by [ansifmt]
      - [line_end]: a newline ([\n])
      - [out]: the standard output ([stdout]) *)
let print_formatted
  : type t.
    ?stylizer:Stylizer.t
    -> ?line_end:string
    -> ?out:out_channel
    -> t
    -> using:(module Interfaces.CONVERTIBLE with type t = t)
    -> unit
  =
  fun ?stylizer ?(line_end = "\n") ?(out = stdout) value ~using:(module M) ->
  Printf.fprintf out "%s%s" (Util.format ?stylizer value ~using:(module M)) line_end
;;
