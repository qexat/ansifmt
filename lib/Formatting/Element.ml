open Internal

type t =
  | Singleton of Token.t
  | Blob of Token.t list
  | Cluster of t list
  | Indented of int * t
  | Parenthesized of pair * t
  | Intercalated of Token.t list * t

and pair =
  | Parentheses
  | Brackets
  | Braces
  | Custom_pair of string * string

let singleton = fun token -> Singleton token
let blob = fun tokens -> Blob tokens
let cluster = fun elements -> Cluster elements

let indented =
  fun ~indent_count:count element ->
  if count < 0 then None else Some (Indented (count, element))
;;

let indented_exn =
  fun ~indent_count:count element ->
  match indented ~indent_count:count element with
  | None -> invalid_arg "arg indent_count must be non-negative"
  | Some element -> element
;;

let parenthesized =
  fun ?(pair = Parentheses) ?(condition = Bool.tautology) element ->
  match condition element with
  | false -> element
  | true -> Parenthesized (pair, element)
;;

let intercalated = fun ~separating elements -> Intercalated (separating, elements)

let sequence =
  fun ~pair elements ->
  elements |> intercalated ~separating:[ Token.comma; Token.space ] |> parenthesized ~pair
;;

let lines = fun elements -> intercalated ~separating:[ Token.line_break ] elements

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
    | Blob tokens ->
      tokens |> List.map (Token.format ~stylizer) |> String.concat "" |> List.singleton
    | Cluster elements -> elements |> List.map (format ~stylizer)
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
