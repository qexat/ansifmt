(** This module defines the escape sequence data type.

    Every value of this type is one styling ([`Bold],
    [`Reverse], [`Foreground] {!Color.red}) or the composition
    of two of them ([`Bold]{{!(&)}[&]}[`Reverse]), which can be chained
    ([`Dim] {{!(&)}[&]} [`Italic] {{!(&)}[&]} [`Blink] {{!(&)}[&]} [`Background] {!Color.magenta}).

    {[
    open Ansifmt

    let chesnay = `Italic
    let rocquencout = Ansi.(`Bold & `Reverse & `Foreground Color.red)

    (* compose to infinity! *)
    let inria = Ansi.(chesnay & rocquencourt)
    ]}

    {i [chesnay], [rocquencourt] and [inria] all have the same
    type {!t}.}

    In a composition chain, if two stylings are overlapping
    (for example, [`Foreground Color.blue & `Foreground Color.red]),
    the rightmost takes precedence.
    *)

(** Represents an ANSI SGR escape sequence. *)
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

(** [compose left right] combines the [left] and right
    sequences into one.

    See also: {{!(&)}[( & )]} *)
val compose : t -> t -> t

(** [serialize ansi] produces a serialized representation of
    the [ansi] escape sequence.

    {b Example}

    {[
    open Ansifmt

    let styling = Ansi.(`Dim & `Background (`Rgb (0, 0, 0)))
    let () = Printf.printf "%s\n" (Ansi.serialize styling)
    ]}

    {i [dim & background(rgb(0, 0, 0))] will be printed.}

    See also: {!deserialize}.
    *)
val serialize : t -> string

(** [deserialize string] produces an escape sequence from a
    serialization [string]. If it fails to parse, returns
    [None].
    
    {b Example}

    {[
    open Ansifmt

    let serialized = "blink & BOLD & foreground(rgb(255, 0, 0))"

    let () =
    match Ansi.deserialize serialized with
    | None -> Printf.eprintf "oh nothing don't worry\n"
    | Some ansi -> 
        Printf.printf "%sHACKED!\n" (Ansi.show ansi)
    ;;
    ]}

    {i A blinking bold [HACKED!] will be printed in red.}

    See also: {!serialize}.
    *)
val deserialize : string -> t option

(** [show ansi] renders the [ansi] escape sequence into a
    string.

    {b Example}

    {[
    open Ansifmt

    let versailles = Ansi.(`Underline & `Background Color.green)
    let () = Printf.printf "%s hello!\n" (Ansi.show versailles)
    ]}
    
    {i [hello!] will be printed with an underline and a green
    background.}

    See also: {!Ansifmt.Fmt}, {!wrap}, {!unshow}.
    *)
val show : t -> string

(** [unshow ansi] renders the ANSI escape sequence that cancels
    [ansi] into a string.

    {b Example}

    {[
    open Ansifmt

    let cergy = Ansi.(`Blink & `Dim & `Foreground Color.blue)

    let () =
    Printf.printf
        "%s hello! %s bye.\n"
        (Ansi.show cergy)
        (Ansi.unshow cergy)
    ;;
    ]}

    {i While [hello!] will be printed dim, blinking and in
    blue, [bye.] will be unstylized.}

    See also: {!Ansifmt.Fmt}, {!wrap}, {!show}.
    *)
val unshow : t -> string

(** [wrap ansi string] wraps [string] with the rendered [ansi]
    escape sequence and its cancelling counterpart.

    For example, if [string] is ["Hello"] and [ansi] is
    [`Bold], the result will be ["\x1b\[1mHello\x1b\[22m"],
    which makes the string appear bold but not what comes
    after.

    {b Example}

    {[
    open Ansifmt

    let styling = `Foreground (`Rgb (255, 127, 0))
    let text = "I love OCaml"

    let () = Printf.printf "%s\n" (Ansi.wrap styling text)
    ]}

    {i [I love OCaml] will be printed in a beautiful orange
    color.}

    See also: {!Ansifmt.Fmt}, {!show}, {!unshow}.
    *)
val wrap : t -> string -> string

(** [left & right] is the same as {!compose} [left right].

    {b NOTE:} [&] is associative but not commutative.

    {b Example}

    {[
    open Ansifmt

    let styling = Ansi.(`Bold & `Foreground Color.yellow)
    ]}
*)
val ( & ) : t -> t -> t

module Attributes : module type of Attributes
module Color : module type of Color
