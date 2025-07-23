(** {1 Color} *)

(** Represents a ANSI color. It can be either 8-bit, which
    provides a portable palette of 256 colors, or 24-bit (RGB),
    which gives more granularity.

    The 256-color palette can be found on the Wikipedia article
    on ANSI escape sequences:
    {:https://en.wikipedia.org/wiki/ANSI_escape_code#8-bit}

    NOTE: Each constructor would normally expect integers
    between 0 and 255 - for convenience, they operate with the
    built-in [int] type which is technically too permissive,
    but allows direct use of numeric literals. To prevent
    generating invalid escape sequences, the integers get
    therefore normalized by taking their absolute value modulo
    256. *)
type t =
  [ `Basic of int
  | `Rgb of int * int * int
  ]

(** [to_attributes ground color] produces the attribute record
    of the [color].

    Since the code depends on whether the color is applied to
    the foreground or the background, this latter piece of
    information must also be provided via [ground]. *)
val to_attributes
  :  [ `Foreground | `Background ]
  -> t
  -> Attributes.t

(** {2 Constants} *)

(** These constants are provided for convenience. They
    correspond to the 4-bit ANSI colors. *)

(** {3 Base colors} *)

val black : t
val red : t
val green : t
val yellow : t
val blue : t
val magenta : t
val cyan : t
val white : t

(** {3 Bright variants} *)

val bright_black : t
val bright_red : t
val bright_green : t
val bright_yellow : t
val bright_blue : t
val bright_magenta : t
val bright_cyan : t
val bright_white : t

(** {2 Checked constructors} *)

(** These functions are equivalent to the type's constructors
    but check the integer values instead of normalizing them. *)

(** [basic index] constructs a 8-bit color. [index] must be
    between 0 and 255 (both included) ; otherwise, it returns
    [None]. *)
val basic : int -> [ `Basic of int ] option

(** [rgb (r, g, b)] constructs an RGB color. All channels must
    be between 0 and 255 (both included) ; otherwise, it
    returns [None]. *)
val rgb : int * int * int -> [ `Rgb of int * int * int ] option

(** {2 Serialization/Deserialization} *)

(** [serialize color] produces a serialized representation of
    the [color].

    {b Tip:} the serialized color can be retreived back using
    {!parse}.

    {b Note:} colors are normalized before serialization. *)
val serialize : t -> string

(** These functions parse strings to get a color from them. *)

(** [of_hex_repr string] attempts to parse the [string] as the
    representation of an hexadecimal number.

    It allows:
    - Leading hash ([#f864a0], optional)
    - Implicitly doubled digits ([#ddd]) *)
val of_hex_repr : string -> [ `Rgb of int * int * int ] option

(** [parse string] attempts to find a serialized color in the
    [string].

    It supports [rgb(r, g, b)] and [basic(n)], ignoring spaces
    and case. Numbers greater than 255 are allowed, but not
    less than 0. *)
val parse : string -> t option

(** {2 Utility functions} *)

(** [luminance color] returns the luminance of the [color] as
    a floating-point number between 0 and 1.

    {b Important:} This is {b NOT} a function to estimate
    perceived lightness. For that, see {!perceived_lightness}. 

    {b Note:} the color is normalized before calculation.

    {b Note:} this function is only available for RGB colors. *)
val luminance : [ `Rgb of int * int * int ] -> float

(** [perceived_lightness color] returns the perceived lightness
    of the [color] as an integer between 0 and 100.

    {b Note:} the color is normalized before calculation.

    {b Note:} this function is only available for RGB colors. *)
val perceived_lightness : [ `Rgb of int * int * int ] -> int

(** [best_for_contrast color] determines whether a light or
    dark opposite color is best.

    For example, if [color] is a background color, this
    function will tell whether the text written on top of it
    should be light or dark for the best readability.

    {b Note:} the calculation is based on
    {{!perceived_lightness}perceived lightness}.

    {b Note:} the color is normalized before calculation.

    {b Note:} this function is only available for RGB colors. *)
val best_for_contrast
  :  [ `Rgb of int * int * int ]
  -> [ `Light | `Dark ]

module Basic : sig
  (** Functions to deal with basic colors. *)

  (** The regular expression used to parse basic colors. *)
  val regular_expression : Re.re

  (** [parse string] attempts to find a serialized basic color
      in the [string]. *)
  val parse : string -> [> `Basic of int ] option
end

module Rgb : sig
  (** Functions to deal with RGB colors. *)

  (** The regular expression used to parse RGB colors. *)
  val regular_expression : Re.re

  (** [parse string] attempts to find a serialized RGB color in
      the [string]. *)
  val parse : string -> [> `Rgb of int * int * int ] option
end
