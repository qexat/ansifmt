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

(** {2 Parsers} *)

(** These functions parse strings to get a color from them. *)

(** [parse string] attempts to find a serialized color in the
    [string].

    It aims to support:
    - Hexadecimal ([#f864a0], [#ddd], [e8a23f]) *)
val parse : string -> [ `Rgb of int * int * int ] option

(** {2 Utility functions} *)

(** [luminance color] returns the luminance of the [color] as
    an integer between 0 and 255.

    {b Important:} This is {b NOT} a function to estimate
    perceived lightness. This is a fast approximation that is
    good enough in terminal emulators.  
    
    {b Note:} the color is normalized before calculation.
    
    {b Note:} this function is only available for RGB colors. *)
val luminance : [ `Rgb of int * int * int ] -> int

(** [best_for_contrast ?threshold color] determines whether a
    light or dark opposite color is best given a [threshold].
    
    For example, if [color] is a background color, this
    function will tell whether the text written on top of it
    should be light or dark for the best readability.
    
    {b Important:} the calculation is based on luminance,
    {b not perceived lightness}. See comment on {!luminance}.
    
    {b Note:} if not provided, the threshold is set to 127.
    It should be a value between 0 and 255.
    A higher threshold will be more likely to suggest [`Light]
    even for lighter [color]s. A lower one will be more likely
    to suggest [`Dark] for lighter [color]s.

    {b Note:} the color is normalized before calculation.

    {b Note:} this function is only available for RGB colors. *)
val best_for_contrast
  :  ?threshold:int
  -> [ `Rgb of int * int * int ]
  -> [ `Light | `Dark ]

(** [serialize color] produces a serialized representation of
    the [color].
    
    {b Note:} colors are normalized before serialization. *)
val serialize : t -> string
