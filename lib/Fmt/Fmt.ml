type t =
  | Composed of t * t
  | Raw of string
  | Stylized of t * Ansi.t

let compose (left : t) (right : t) : t = Composed (left, right)
let raw (string : string) : t = Raw string
let stylize (fmt : t) (ansi : Ansi.t) : t = Stylized (fmt, ansi)

let join ~(on : t) (fmts : t list) : t =
  match fmts with
  | [] -> on
  | first :: rest ->
    List.fold_left
      (fun left right -> Composed (Composed (left, on), right))
      first
      rest
;;

let rec render ~(with_styling : bool) (fmt : t) : string =
  match fmt with
  | Composed (left, right) ->
    render ~with_styling left ^ render ~with_styling right
  | Raw string -> string
  | Stylized (fmt', ansi) ->
    if with_styling
    then Ansi.wrap ansi (render ~with_styling fmt')
    else render ~with_styling fmt'
;;

let print
      ?(out = stdout)
      ?(ending = Some "\n")
      ?with_styling:(color_strategy = `Auto)
      pretty
  =
  let with_styling =
    match color_strategy with
    | `Always -> true
    | `Never -> false
    | `Auto -> Out_channel.isatty out
  in
  let ending = Option.value ending ~default:"" in
  Printf.fprintf out "%s%s" (render ~with_styling pretty) ending
;;

let rec serialize (fmt : t) : string =
  match fmt with
  | Composed (left, right) ->
    Printf.sprintf "%s ++ %s" (serialize left) (serialize right)
  | Raw string ->
    Printf.sprintf "raw(\"%s\")" (String.escaped string)
  | Stylized (fmt', ansi) ->
    Printf.sprintf
      "stylized(%s, %s)"
      (serialize fmt')
      (Ansi.serialize ansi)
;;

let show (fmt : t) : t = fmt
let ( ++ ) = compose
