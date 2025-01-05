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
    -> ?line_end:string
    -> ?out:out_channel
    -> t
    -> using:(module Formatting.TOKENIZABLE with type t = t)
    -> unit
  =
  fun ?(stylizer = Formatting.Stylizer.default)
    ?(line_end = "\n")
    ?(out = stdout)
    value
    ~using:(module M) ->
  Printf.fprintf out "%s%s" (Formatting.format ~stylizer value ~using:(module M)) line_end
;;
