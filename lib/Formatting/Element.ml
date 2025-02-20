open Internal

(** Represents a formatting element. *)
type t =
  | Singleton of Token.t
  | Blob of Token.t list
  | Cluster of t list
  | Indented of int * t
  | Parenthesized of pair * t
  | Intercalated of Token.t list * t

(** Represents a pair of characters (e.g. parentheses). *)
and pair =
  | Parentheses
  | Brackets
  | Braces
  | Custom_pair of string * string

(** [singleton token] creates a singleton element of [token]. *)
let singleton = fun token -> Singleton token

(** [blob tokens] groups [tokens] into an element. *)
let blob = fun tokens -> Blob tokens

(** [cluster elements] groups [elements] together. *)
let cluster = fun elements -> Cluster elements

(** [indented ~indent_count element] produces a new element
  that will be indented [indent_count] times when formatting
  it. Returns [None] if [indent_count] is negative. *)
let indented =
  fun ~indent_count:count element ->
  if count < 0 then None else Some (Indented (count, element))
;;

(** [indented_exn ~indent_count element] produces a new
  element that will be indented [indent_count] times when
  formatting it. Raises [Invalid_arg] if [indent_count] is
  negative. *)
let indented_exn =
  fun ~indent_count:count element ->
  match indented ~indent_count:count element with
  | None -> invalid_arg "arg indent_count must be non-negative"
  | Some element -> element
;;

(** [parenthsized ~pair element] produces a new element that
  will be surrounded by a [pair] when formatting it. *)
let parenthesized = fun ~pair element -> Parenthesized (pair, element)

(** [intercalated ~separating elements] produces a new element
  where the [separating] list of tokens is intercalated
  between the [elments]. *)
let intercalated = fun ~separating elements -> Intercalated (separating, elements)

(** [sequence ~pair elements] produces a new element where the
  [elements] are separated by a comma (and a space) and
  surrounded by [pair]. *)
let sequence =
  fun ~pair elements ->
  elements |> intercalated ~separating:[ Token.comma; Token.space ] |> parenthesized ~pair
;;

(** [lines elements] produces a new element that intersperses
  a line break between [elements] when formatting it,
  WITHOUT a trailing newline. *)
let lines = fun elements -> intercalated ~separating:[ Token.line_break ] elements

(** [format ~stylizer element] renders the [element] into a
  string using the [stylizer]. *)
let rec format =
  fun ?(stylizer = Stylizer.default) element ->
  format_blocks ~stylizer element |> String.concat ""

and format_blocks =
  let indent ~count line = String.make (count * 2) ' ' ^ line in
  let render_pair = function
    | Parentheses -> "(", ")"
    | Brackets -> "[", "]"
    | Braces -> "{", "}"
    | Custom_pair (left, right) -> left, right
  in
  fun ?(stylizer = Stylizer.default) element ->
    element
    |> function
    | Singleton token -> [ Token.format ~stylizer token ]
    | Blob tokens -> tokens |> List.map (Token.format ~stylizer)
    | Cluster elements -> elements |> List.map (format_blocks ~stylizer) |> List.flatten
    | Indented (count, element') ->
      format ~stylizer element'
      |> String.split_on_char '\n'
      |> List.map (indent ~count)
      |> List.intersperse "\n"
    | Parenthesized (pair, element') ->
      let left, right = render_pair pair in
      (left :: format_blocks ~stylizer element') @ [ right ]
    | Intercalated (separating, element') ->
      List.intersperse (format (Blob separating)) (format_blocks element')
;;
