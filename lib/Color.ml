(** [Color] encodes ANSI colors.

  It comes in three flavors:
  - 4-bit ([Minimal])
  - 8-bit ([Advanced])
  - 24-bit ([Rgb]) *)

open Util

module Ground = struct
  (** [Ground] encodes the information on whether a certain
      color is on the foreground or the background. *)

  type t =
    [ `Foreground
    | `Background
    ]

  (** [to_int ?bright ground] produces the corresponding leading
      digit for an SGR escape sequence to be set as foreground
      or background. *)
  let to_int ?(bright : bool = false) (ground : t) : int =
    (* We could also match only on [ground] and add 6 when
    [bright] is true, but I like to avoid arithmetic code when
    possible as it makes it more obscure - constants are easier
    to reason about and less error prone. *)
    match ground, bright with
    | `Foreground, false -> 3
    | `Background, false -> 4
    | `Foreground, true -> 9
    | `Background, true -> 10
  ;;
end

module Minimal = struct
  (** [Minimal] encodes the 8 default ANSI colors. *)

  type t =
    | Black
    | Red
    | Green
    | Yellow
    | Blue
    | Magenta
    | Cyan
    | White

  (** [to_int color] produces the corresponding ANSI SGR code
      of the [color], which is a value between 0 and 7. *)
  let to_int : t -> int = function
    | Black -> 0
    | Red -> 1
    | Green -> 2
    | Yellow -> 3
    | Blue -> 4
    | Magenta -> 5
    | Cyan -> 6
    | White -> 7
  ;;
end

type t =
  [ (* Minimal could also be a tuple, but I like the explicitness
     of records. *)
    `Minimal of minimal
  | `Advanced of Int8.t
  | (* Here, I don't think a record is needed - the order of the
     channels are literally given out by the constructor's name. *)
    `Rgb of Int8.t * Int8.t * Int8.t
  ]

and minimal =
  { color : Minimal.t
  ; bright : bool
  }

(* It's handy for the user to have module-level constants for
   each minimal color. *)

(** Default black color. *)
let black : t = `Minimal { color = Minimal.Black; bright = false }

(** Default red color. *)
let red : t = `Minimal { color = Minimal.Red; bright = false }

(** Default green color. *)
let green : t = `Minimal { color = Minimal.Green; bright = false }

(** Default yellow color. *)
let yellow : t = `Minimal { color = Minimal.Yellow; bright = false }

(** Default blue color. *)
let blue : t = `Minimal { color = Minimal.Blue; bright = false }

(** Default magenta color. *)
let magenta : t = `Minimal { color = Minimal.Magenta; bright = false }

(** Default cyan color. *)
let cyan : t = `Minimal { color = Minimal.Cyan; bright = false }

(** Default white color. *)
let white : t = `Minimal { color = Minimal.White; bright = false }

(** Default bright black (gray) color. *)
let bright_black : t = `Minimal { color = Minimal.Black; bright = true }

(** Default bright red color. *)
let bright_red : t = `Minimal { color = Minimal.Red; bright = true }

(** Default bright green color. *)
let bright_green : t = `Minimal { color = Minimal.Green; bright = true }

(** Default bright yellow color. *)
let bright_yellow : t = `Minimal { color = Minimal.Yellow; bright = true }

(** Default bright blue color. *)
let bright_blue : t = `Minimal { color = Minimal.Blue; bright = true }

(** Default bright magenta color. *)
let bright_magenta : t = `Minimal { color = Minimal.Magenta; bright = true }

(** Default bright cyan color. *)
let bright_cyan : t = `Minimal { color = Minimal.Cyan; bright = true }

(** Default bright white color. *)
let bright_white : t = `Minimal { color = Minimal.White; bright = true }

(* It's also useful for the user to have module-level functions
   to easily create colors without knowing the intricacies and
   details of their implementation. *)

(** [make_minimal ?bright value] creates a minimal color.
    If [value] is not a valid color code, it returns [None].

    A valid color code is an integer i where 0 <= i <= 7. *)
let make_minimal ?(bright : bool = false) : int -> [ `Minimal of minimal ] option
  = function
  | 0 -> Some (`Minimal { color = Minimal.Black; bright })
  | 1 -> Some (`Minimal { color = Minimal.Red; bright })
  | 2 -> Some (`Minimal { color = Minimal.Green; bright })
  | 3 -> Some (`Minimal { color = Minimal.Yellow; bright })
  | 4 -> Some (`Minimal { color = Minimal.Blue; bright })
  | 5 -> Some (`Minimal { color = Minimal.Magenta; bright })
  | 6 -> Some (`Minimal { color = Minimal.Cyan; bright })
  | 7 -> Some (`Minimal { color = Minimal.White; bright })
  | _ -> None
;;

(** [make_minimal_exn ?bright value] creates a minimal color.
    If [value] is not a valid color code, it raises a [Failure]
    exception.

    A valid color code is an integer i where 0 <= i <= 7. *)
let make_minimal_exn ?(bright : bool = false) (value : int) : [ `Minimal of minimal ] =
  match make_minimal ~bright value with
  | None -> failwith "value must be an integer between 0 and 7 (both included)"
  | Some color -> color
;;

(** [make_advanced value] creates an advanced color.
    If [value] is not a valid color code, it returns [None].

    A valid color code is an integer i where 0 <= i < 256. *)
let make_advanced (value : int) : [ `Advanced of Int8.t ] option =
  Option.map (fun (value : Int8.t) -> `Advanced value) (Int8.of_int value)
;;

(** [make_advanced_exn value] creates an advanced color.
    If [value] is not a valid color code, it raises a [Failure]
    exception.

    A valid color code is an integer i where 0 <= i < 256. *)
let make_advanced_exn (value : int) : [ `Advanced of Int8.t ] =
  `Advanced (Int8.of_int_exn value)
;;

module Channel = struct
  (** Represents an RGB channel - either red, green, or blue. *)
  type t =
    | Red of int
    | Green of int
    | Blue of int

  (** [name channel] returns the name of the [channel]. *)
  let name : t -> string = function
    | Red _ -> "red"
    | Green _ -> "green"
    | Blue _ -> "blue"
  ;;

  (** [value channel] retreives the underlying value of the
      [channel]. *)
  let value : t -> int = function
    | Red value -> value
    | Green value -> value
    | Blue value -> value
  ;;

  (** [to_int8 channel] tries to convert the [channel]'s value
      into a [Int8] value.

      If it fails, it returns the channel data wrapped in the
      [Error] variant. This is because this function is often
      applied in batch to every channel of an RGB component, so
      the user can trace which channel's value was invalid. *)
  let to_int8 (channel : t) : (Int8.t, t) result =
    Option.to_result ~none:channel (Int8.of_int (value channel))
  ;;

  (** [red value] creates a [Red] channel. *)
  let red (value : int) : t = Red value

  (** [green value] creates a [Green] channel. *)
  let green (value : int) : t = Green value

  (** [blue value] creates a [Blue] channel. *)
  let blue (value : int) : t = Blue value
end

(** [make_rgb red green blue] creates an RGB color.
    If any of [red], [green] or [blue] is not a valid channel
    value, it returns an [Error] which indicates which channel
    had an invalid value.

    A valid channel value is an integer i where 0 <= i < 256.

    For the version that returns an [option] instead, see
    [make_rgb_opt]. *)
let make_rgb (red : int) (green : int) (blue : int)
  : ([ `Rgb of Int8.t * Int8.t * Int8.t ], Channel.t) result
  =
  (* We convert each channel independently by mapping them to
  some specialized type so we can extract the information of
  what the first channel with an incorrect value was.
  That information is used for example in [make_rgb_exn]. *)
  (red, green, blue)
  |> Triplet.map Channel.red Channel.green Channel.blue
  |> Triplet.map_uniform ~func:Channel.to_int8
  |> Triplet.all_ok
  |> Result.map (fun (r, g, b) -> `Rgb (r, g, b))
;;

(** [make_rgb_opt red green blue] creates an RGB color.
    If any of [red], [green] or [blue] is not a valid channel
    value, it returns [None].

    A valid channel value is an integer i where 0 <= i < 256.

    For the version that returns a result with the invalid
    channel data, see [make_rgb]. *)
let make_rgb_opt (red : int) (green : int) (blue : int)
  : [ `Rgb of Int8.t * Int8.t * Int8.t ] option
  =
  Result.to_option (make_rgb red green blue)
;;

(** [make_rgb_exn red green blue] creates an RGB color.
    If any of [red], [green] or [blue] is not a valid channel
    value, it raises a [Failure] exception.

    A valid channel value is an integer i where 0 <= i < 256. *)
let make_rgb_exn (red : int) (green : int) (blue : int)
  : [ `Rgb of Int8.t * Int8.t * Int8.t ]
  =
  match make_rgb red green blue with
  | Ok color -> color
  | Error channel ->
    failwith
      (Printf.sprintf
         "channel %s has the incorrect value %d (must be between 0 and 255)"
         (Channel.name channel)
         (Channel.value channel))
;;

(** [make_rgb_hex str] parses an RGB color from a string [str] like 7057fc or #fbca04 *)
let make_rgb_hex (hex : string) : [ `Rgb of Int8.t * Int8.t * Int8.t ] option =
  let ( let* ) = Option.bind in
  let of_hex_digit c =
    match c with
    | '0' .. '9' -> Some (Char.code c - Char.code '0')
    | 'a' .. 'f' -> Some (Char.code c - Char.code 'a' + 10)
    | _ -> None
  in
  let parse_channel char1 char2 =
    let* digit1 = of_hex_digit char1 in
    let* digit2 = of_hex_digit char2 in
    Some ((digit1 * 16) + digit2)
  in
  let parse_hex = function
    | [ r1; r2; g1; g2; b1; b2 ] ->
      let* red = parse_channel r1 r2 in
      let* green = parse_channel g1 g2 in
      let* blue = parse_channel b1 b2 in
      make_rgb_opt red green blue
    | _ -> None
  in
  let parse_hex_color = function
    | '#' :: hex -> parse_hex hex
    | hex -> parse_hex hex
  in
  hex |> String.lowercase_ascii |> String.to_seq |> List.of_seq |> parse_hex_color
;;

(** [to_ansi color] produces an SGR escape portion that can be
    embedded in a string based on the [color]. *)
let to_ansi ~(ground : Ground.t) : t -> string = function
  | `Minimal { color; bright } ->
    Printf.sprintf "%d%d" (Ground.to_int ~bright ground) (Minimal.to_int color)
  | `Advanced color ->
    Printf.sprintf "%d8;5;%d" (Ground.to_int ground) (Int8.to_int color)
  | `Rgb (r, g, b) ->
    Printf.sprintf
      "%d8;2;%d;%d;%d"
      (Ground.to_int ground)
      (Int8.to_int r)
      (Int8.to_int g)
      (Int8.to_int b)
;;

(** This function returns luminance of an RGB color in range between 0 and 255.

See {!best_for_contrast} for a usage example. *)
let luminance : [ `Rgb of Int8.t * Int8.t * Int8.t ] -> int = function
  | `Rgb (r, g, b) ->
    let r = Int8.to_int r in
    let g = Int8.to_int g in
    let b = Int8.to_int b in
    ((2126 * r) + (7152 * g) + (722 * b)) / 10000
;;

(** [best_for_contrast ~threshold rgb] takes a color [rgb], [threshold] between
0 and 255 (both inclusive) representing luminescence tolerance, and returns the
suggested color theme for the opposite color to achieve the best contrast.

For example, if [rgb] is a background colour, and [best_for_contrast] return
[`Light], you should select a light foreground colour for the best readability.

- {b NOTE}: Default [threshold] value is 128.
- {b NOTE}: Higher [threshold] will suggest [`Light] more often and on brighter
  colours. Lower [threshold] will suggest [`Light] for darker colours. The
  opposite is true for [`Dark].
*)
let best_for_contrast ?(threshold = 128)
  : [ `Rgb of Int8.t * Int8.t * Int8.t ] -> [ `Light | `Dark ]
  =
  fun rgb -> if luminance rgb < threshold then `Light else `Dark
;;
