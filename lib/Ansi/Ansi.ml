type t =
  [ `Bold
  | `Dim
  | `Italic
  | `Underline
  | `Blink
  | `Reverse
  | `Foreground of Color.t
  | `Background of Color.t
  | `Composed of t * t
  ]

let rec ( = ) (left : t) (right : t) =
  match (left, right) with
  | (`Bold, `Bold)
  | (`Dim, `Dim)
  | (`Italic, `Italic)
  | (`Underline, `Underline)
  | (`Blink, `Blink)
  | (`Reverse, `Reverse) -> true
  | (`Foreground left_color, `Foreground right_color) ->
    Color.(left_color = right_color)
  | (`Background left_color, `Background right_color) ->
    Color.(left_color = right_color)
  | (`Composed (left1, left2), `Composed (right1, right2)) ->
    left1 = right1 && left2 = right2
  | (_, _) -> false
;;

let foreground color = `Foreground color
let background color = `Background color
let compose (left : t) (right : t) : t = `Composed (left, right)

let rec to_attributes (ansi : t) : Attributes.t =
  match ansi with
  | `Bold -> Attributes.create 1
  | `Dim -> Attributes.create 2
  | `Italic -> Attributes.create 3
  | `Underline -> Attributes.create 4
  | `Blink -> Attributes.create 5
  | `Reverse -> Attributes.create 7
  | `Foreground color -> Color.to_attributes `Foreground color
  | `Background color -> Color.to_attributes `Background color
  | `Composed (left, right) ->
    Attributes.(to_attributes left & to_attributes right)
;;

let rec to_cancelling_attributes (ansi : t) : Attributes.t =
  match ansi with
  | `Bold -> Attributes.create 22
  | `Dim -> Attributes.create 22
  | `Italic -> Attributes.create 23
  | `Underline -> Attributes.create 24
  | `Blink -> Attributes.create 25
  | `Reverse -> Attributes.create 27
  | `Foreground _ -> Attributes.create 39
  | `Background _ -> Attributes.create 49
  | `Composed (left, right) ->
    Attributes.(
      to_cancelling_attributes left
      & to_cancelling_attributes right)
;;

let rec serialize (ansi : t) : string =
  match ansi with
  | `Bold -> "bold"
  | `Dim -> "dim"
  | `Italic -> "italic"
  | `Underline -> "underline"
  | `Blink -> "blink"
  | `Reverse -> "reverse"
  | `Foreground color ->
    Printf.sprintf "foreground(%s)" (Color.serialize color)
  | `Background color ->
    Printf.sprintf "background(%s)" (Color.serialize color)
  | `Composed (left, right) ->
    Printf.sprintf "%s & %s" (serialize left) (serialize right)
;;

module Option = struct
  include Option

  let ( let+ ) = bind

  let rec all = function
    | [] -> None
    | Some item :: [] -> Some [ item ]
    | None :: _ -> None
    | Some item :: rest ->
      let+ rest' = all rest in
      Some (item :: rest')
  ;;
end

let ( let+ ) = Option.bind

let rec list_to_composed = function
  | [] -> None
  | first :: [] -> Some first
  | first :: rest ->
    let+ right = list_to_composed rest in
    Some (compose first right)
;;

module Deserialization = struct
  let try_remove_xground source ~x =
    let xground = x ^ "ground" in
    let xg = String.sub x 0 1 ^ "g" in
    if String.starts_with ~prefix:xground source
    then (
      let xground_length = String.length xground in
      Some
        (String.sub
           source
           xground_length
           (String.length source - xground_length)
         |> String.trim))
    else if String.starts_with ~prefix:xg source
    then
      Some
        (String.sub source 2 (String.length source - 2)
         |> String.trim)
    else None
  ;;

  let try_remove_paren_left source =
    if String.starts_with ~prefix:"(" source
    then Some (String.sub source 1 (String.length source - 1))
    else None
  ;;

  let try_remove_paren_right source =
    if String.ends_with ~suffix:")" source
    then Some (String.sub source 0 (String.length source - 1))
    else None
  ;;

  let try_parse_xground source ~f ~x =
    let+ source = try_remove_xground source ~x in
    let+ source = try_remove_paren_left source in
    let+ source = try_remove_paren_right source in
    let+ color = Color.parse source in
    Some (f color)
  ;;

  let parse_single source =
    match source with
    | "bold" -> Some `Bold
    | "dim" -> Some `Dim
    | "italic" -> Some `Italic
    | "underline" -> Some `Underline
    | "blink" -> Some `Blink
    | "reverse" -> Some `Reverse
    | _ when String.starts_with ~prefix:"background" source ->
      try_parse_xground source ~x:"back" ~f:background
    | _ -> try_parse_xground source ~x:"fore" ~f:foreground
  ;;
end

let deserialize (source : string) : t option =
  let+ parts =
    source
    |> String.split_on_char '&'
    |> List.map String.trim
    |> List.map String.lowercase_ascii
    |> List.map Deserialization.parse_single
    |> Option.all
  in
  list_to_composed parts
;;

let show (ansi : t) : string =
  Printf.sprintf
    "\x1b[%sm"
    (Attributes.show (to_attributes ansi))
;;

let unshow (ansi : t) : string =
  Printf.sprintf
    "\x1b[%sm"
    (Attributes.show (to_cancelling_attributes ansi))
;;

let wrap (ansi : t) (string : string) : string =
  let cancelling_ansi_string = unshow ansi in
  if String.(equal string empty)
  then
    (* As the string is empty, the [ansi] escape sequence will
       not affect anything, meaning we can safely drop it.
       However, we cannot do the same for the cancelling part,
       as there might be outer active escape sequences still
       running. *)
    cancelling_ansi_string
  else
    Printf.sprintf
      "%s%s%s"
      (show ansi)
      string
      cancelling_ansi_string
;;

let enrich = wrap
let ( & ) = compose

module Attributes = Attributes
module Color = Color
