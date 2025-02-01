(** [Styling] encodes terminal styling as a CSS-like language. *)

type t =
  { foreground : Color.t option
  ; background : Color.t option
  ; bold : bool
  ; dim : bool
  ; italic : bool
  ; underlined : bool
  }

let none : t =
  { foreground = None
  ; background = None
  ; bold = false
  ; dim = false
  ; italic = false
  ; underlined = false
  }
;;

let create
      ?(foreground : [< Color.t ] option)
      ?(background : [< Color.t ] option)
      ?(bold : bool = false)
      ?(dim : bool = false)
      ?(italic : bool = false)
      ?(underlined : bool = false)
      ()
  : t
  =
  let foreground = (foreground :> Color.t option) in
  let background = (background :> Color.t option) in
  { foreground; background; bold; dim; italic; underlined }
;;

let fg : [< Color.t ] -> t = fun foreground -> create ~foreground ()
let bg : [< Color.t ] -> t = fun background -> create ~background ()
let bold : t = { none with bold = true }
let dim : t = { none with dim = true }
let italic : t = { none with italic = true }
let underlined : t = { none with underlined = true }

let ( & ) left right =
  let last x y =
    match x, y with
    | _, Some _ -> y
    | _, _ -> x
  in
  let foreground = last left.foreground right.foreground in
  let background = last left.background right.background in
  let bold = left.bold || right.bold in
  let dim = left.dim || right.dim in
  let italic = left.italic || right.italic in
  let underlined = left.underlined || right.underlined in
  { foreground; background; bold; dim; italic; underlined }
;;

let make_sgr_sequence (inner : string) : string = "\x1b[" ^ inner ^ "m"

let add_color_to_buffer
      (buffer : Buffer.t)
      (color : Color.t option)
      ~(ground : Color.Ground.t)
  : unit
  =
  match color with
  | None -> ()
  | Some color' ->
    Buffer.add_string buffer (make_sgr_sequence (Color.to_ansi ~ground color'))
;;

let to_ansi : t -> string = function
  | { foreground; background; bold; dim; italic; underlined } ->
    let buffer = Buffer.create 16 in
    if bold then Buffer.add_string buffer (make_sgr_sequence "1");
    if dim then Buffer.add_string buffer (make_sgr_sequence "2");
    if italic then Buffer.add_string buffer (make_sgr_sequence "3");
    if underlined then Buffer.add_string buffer (make_sgr_sequence "4");
    add_color_to_buffer buffer foreground ~ground:`Foreground;
    add_color_to_buffer buffer background ~ground:`Background;
    Buffer.contents buffer
;;

let wrap : contents:string -> t -> string =
  fun ~contents -> function
  | { foreground; background; bold; dim; italic; underlined } as styling ->
    let buffer = Buffer.create 16 in
    if bold || dim then Buffer.add_string buffer (make_sgr_sequence "22");
    if italic then Buffer.add_string buffer (make_sgr_sequence "23");
    if underlined then Buffer.add_string buffer (make_sgr_sequence "24");
    if Option.is_some foreground then Buffer.add_string buffer (make_sgr_sequence "39");
    if Option.is_some background then Buffer.add_string buffer (make_sgr_sequence "49");
    to_ansi styling ^ contents ^ Buffer.contents buffer
;;
