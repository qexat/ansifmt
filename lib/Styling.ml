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
      ?(foreground : Color.t option)
      ?(background : Color.t option)
      ?(bold : bool = false)
      ?(dim : bool = false)
      ?(italic : bool = false)
      ?(underlined : bool = false)
      ()
  : t
  =
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
