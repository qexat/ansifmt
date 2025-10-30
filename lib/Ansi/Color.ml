type t =
  [ `Basic of int
  | `Rgb of int * int * int
  ]

let ( = ) (left : t) (right : t) =
  match (left, right) with
  | (`Basic i, `Basic j) -> Int.equal i j
  | (`Rgb (r1, g1, b1), `Rgb (r2, g2, b2)) ->
    Int.equal r1 r2 && Int.equal g1 g2 && Int.equal b1 b2
  | (_, _) -> false
;;

let normalize_value (value : int) : int = abs value mod 256

let normalize_rgb (color : [ `Rgb of int * int * int ])
  : [> `Rgb of int * int * int ]
  =
  match color with
  | `Rgb (r, g, b) ->
    `Rgb
      (normalize_value r, normalize_value g, normalize_value b)
;;

let normalize (color : t) : t =
  match color with
  | `Basic index -> `Basic (normalize_value index)
  | `Rgb _ as color -> normalize_rgb color
;;

let to_attributes
      (ground : [ `Foreground | `Background ])
      (color : t)
  : Attributes.t
  =
  let code =
    match ground with
    | `Foreground -> 38
    | `Background -> 48
  in
  let parameters =
    match normalize color with
    | `Basic index -> [ 5; index ]
    | `Rgb (r, g, b) -> [ 2; r; g; b ]
  in
  Attributes.create ~parameters code
;;

let black : t = `Basic 0
let red : t = `Basic 1
let green : t = `Basic 2
let yellow : t = `Basic 3
let blue : t = `Basic 4
let magenta : t = `Basic 5
let cyan : t = `Basic 6
let white : t = `Basic 7
let bright_black : t = `Basic 8
let bright_red : t = `Basic 9
let bright_green : t = `Basic 10
let bright_yellow : t = `Basic 11
let bright_blue : t = `Basic 12
let bright_magenta : t = `Basic 13
let bright_cyan : t = `Basic 14
let bright_white : t = `Basic 15

let basic : int -> [ `Basic of int ] option =
  fun index ->
  if Int.(equal index (normalize_value index))
  then Some (`Basic index)
  else None
;;

let rgb : int * int * int -> [ `Rgb of int * int * int ] option =
  fun (r, g, b) ->
  if
    Int.(equal r (normalize_value r))
    && Int.(equal g (normalize_value g))
    && Int.(equal b (normalize_value b))
  then Some (`Rgb (r, g, b))
  else None
;;

let ( let+ ) = Option.bind

let of_hex_repr : string -> [ `Rgb of int * int * int ] option =
  let parse_hex_digit (char : char) : int option =
    match char with
    | '0' .. '9' -> Some (Char.code char - Char.code '0')
    | 'A' .. 'F' -> Some (Char.code char - Char.code 'A' + 10)
    | 'a' .. 'f' -> Some (Char.code char - Char.code 'a' + 10)
    | _ -> None
  in
  let parse_channel (char0 : char) (char1 : char) : int option =
    let+ digit0 = parse_hex_digit char0 in
    let+ digit1 = parse_hex_digit char1 in
    Some ((digit0 * 0x10) + digit1)
  in
  let rec try_parse_hex = function
    | [ r; g; b ] -> try_parse_hex [ r; r; g; g; b; b ]
    | [ r0; r1; g0; g1; b0; b1 ] ->
      let+ red = parse_channel r0 r1 in
      let+ green = parse_channel g0 g1 in
      let+ blue = parse_channel b0 b1 in
      rgb (red, green, blue)
    | _ -> None
  in
  fun string ->
    string
    |> String.trim
    |> String.to_seq
    |> List.of_seq
    |> function
    | '#' :: rest -> try_parse_hex rest
    | rest -> try_parse_hex rest
;;

module Basic = struct
  let regular_expression =
    Re.compile
    @@ Re.Pcre.re
         ~flags:[ `CASELESS ]
         {|^\s*basic\s*\(\s*(0|[1-9][0-9]*)\s*\)\s*$|}
  ;;

  let parse : string -> [> `Basic of int ] option =
    fun string ->
    let+ match' = Re.exec_opt regular_expression string in
    let+ group = Re.Group.get_opt match' 1 in
    let+ index = int_of_string_opt group in
    Some (`Basic index)
  ;;
end

module Rgb = struct
  let regular_expression =
    Re.compile
    @@ Re.Pcre.re
         ~flags:[ `CASELESS ]
         {|^\s*rgb\s*\(\s*(0|[1-9][0-9]*)\s*,\s*(0|[1-9][0-9]*)\s*,\s*(0|[1-9][0-9]*)\s*(,\s*)?\)\s*$|}
  ;;

  let parse : string -> [> `Rgb of int * int * int ] option =
    fun string ->
    let+ match' = Re.exec_opt regular_expression string in
    let+ red_group = Re.Group.get_opt match' 1 in
    let+ green_group = Re.Group.get_opt match' 2 in
    let+ blue_group = Re.Group.get_opt match' 3 in
    let+ red = int_of_string_opt red_group in
    let+ green = int_of_string_opt green_group in
    let+ blue = int_of_string_opt blue_group in
    Some (`Rgb (red, green, blue))
  ;;
end

let parse : string -> t option =
  let ( let- )
    : type a. a option -> (unit -> a option) -> a option
    =
    fun option func ->
    match option with
    | None -> func ()
    | Some _ -> option
  in
  fun string ->
    let- () = Basic.parse string in
    Rgb.parse string
;;

(* Linearization, luminance and perceived lightness algorithms
   are mostly based on the following StackOverflow comment:
   <https://stackoverflow.com/a/56678483>, with some
   adjustements. *)

let linearize
  : [ `Rgb of int * int * int ] -> float * float * float
  =
  let linearize_channel channel =
    let channel_srgb = Int.to_float channel /. 255. in
    if channel_srgb <= 0.04045
    then channel_srgb /. 12.92
    else ((channel_srgb +. 0.055) /. 1.055) ** 2.4
  in
  fun rgb ->
    match rgb with
    | `Rgb (r, g, b) ->
      ( linearize_channel r
      , linearize_channel g
      , linearize_channel b )
;;

let luminance : [ `Rgb of int * int * int ] -> float =
  fun rgb ->
  let (r, g, b) = linearize (normalize_rgb rgb) in
  (0.2126 *. r) +. (0.7152 *. g) +. (0.0722 *. b)
;;

let perceived_lightness : [ `Rgb of int * int * int ] -> int =
  fun rgb ->
  let luminance = luminance rgb in
  (if luminance <= 216. /. 24389.
   then luminance *. 903.3
   else ((luminance ** (1. /. 3.)) *. 116.) -. 16.)
  |> Float.to_int
;;

let best_for_contrast
  : [ `Rgb of int * int * int ] -> [ `Light | `Dark ]
  =
  fun rgb ->
  if perceived_lightness rgb < 50 then `Light else `Dark
;;

let serialize : t -> string =
  fun color ->
  match normalize color with
  | `Basic n -> Printf.sprintf "basic(%d)" n
  | `Rgb (r, g, b) -> Printf.sprintf "rgb(%d, %d, %d)" r g b
;;
