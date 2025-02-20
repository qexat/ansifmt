open Util

module Token_type = struct
  type t =
    | Comment
    | Documentation
    | Keyword
    | Literal_constant
    | Literal_string
    | String_template
    | Identifier
    | Constant (* for languages with mutable identifiers by default *)
    | Parameter
    | Variable_type
    | Variable_lifetime
    | Function
    | Method
    | Procedure
    | Function_special (* e.g. built-in functions *)
    | Method_special (* e.g. Python's dunder methods *)
    | Type
    | Class
    | Struct (* or record *)
    | Trait (* or typeclass *)
    | Macro
    | Module (* or namespace *)
    | Operator_expr (* e.g. `*` in `8 * 5` *)
    | Operator_stmt (* e.g. `=` in `let x = 3` *)
    | Operator_special
    | Pair (* also called "bracket" *)
    | Punctuation_strong
    | Punctuation_weak
    | Space
    | Indent
    | Line_break
    | Custom of Styling.t
end

module Stylizer = struct
  type t = Token_type.t -> Styling.t

  let default : t =
    let open Token_type in
    function
    | Comment | Documentation -> Styling.create ~dim:true ()
    | Keyword | Punctuation_strong ->
      Styling.create ~foreground:Color.magenta ~bold:true ()
    | Operator_stmt | Operator_special -> Styling.create ~foreground:Color.magenta ()
    | Identifier -> Styling.create ~foreground:Color.cyan ()
    | Parameter -> Styling.create ~foreground:Color.cyan ~italic:true ()
    | Constant | Literal_constant | Function_special ->
      Styling.create ~foreground:(Color.make_rgb_exn 153 51 204) ()
    | Function | Method | Method_special | Procedure | Operator_expr ->
      Styling.create ~foreground:Color.blue ()
    | Variable_type | Variable_lifetime | Type | Class | Trait | Struct | Module ->
      Styling.create ~foreground:Color.yellow ()
    | Macro -> Styling.create ~foreground:Color.red ()
    | Literal_string | String_template -> Styling.create ~foreground:Color.green ()
    | Pair | Punctuation_weak | Space | Indent | Line_break -> Styling.none
    | Custom styling -> styling
  ;;
end

module Token = struct
  (** [Token] is the formatter token data type. *)

  (** A token is simply a pair (token type, lexeme). *)
  type t = Token_type.t * string

  (** Token representing a single whitespace.

      Exists as a constant due to its recurrent use. *)
  let space : t = Token_type.Space, " "

  (** Token representing a line break.
      It is useful if you want to have a potential newline if
      it does not fit in one line.

      Exists as a constant due to its recurrent use. *)
  let line_break : t = Token_type.Line_break, "\n"

  (** Token representing a comma.

      Exists as a constant due to its recurrent use. *)
  let comma : t = Token_type.Punctuation_weak, ","

  (** Token representing a colon.

      Exists as a constant due to its recurrent use. *)
  let colon : t = Token_type.Punctuation_strong, ":"

  let format ?(stylizer : Stylizer.t = Stylizer.default) : t -> string =
    fun (token_type, lexeme) -> Styling.wrap ~contents:lexeme (stylizer token_type)
  ;;
end

module Tree = struct
  type t =
    | Simple of Token.t list
    | Parenthesized of t
    | Block of t list

  let simple : Token.t list -> t = fun tokens -> Simple tokens
  let parenthesized : t -> t = fun tree -> Parenthesized tree
  let block : t list -> t = fun trees -> Block trees

  let parenthesize_if : ('a -> bool) -> ('a -> t) -> 'a -> t =
    fun predicate tokenizer value ->
    let base = tokenizer value in
    if predicate value then parenthesized base else base
  ;;

  let rec format : ?parentheses:string * string -> ?stylizer:Stylizer.t -> t -> string =
    fun ?(parentheses = "(", ")") ?(stylizer = Stylizer.default) tree ->
    let opening_token, closing_token =
      Pair.map_uniform (fun lexeme -> Token_type.Pair, lexeme) parentheses
    in
    tree
    |> function
    | Simple tokens -> tokens |> List.map (Token.format ~stylizer) |> String.concat ""
    | Parenthesized subtree ->
      Token.format ~stylizer opening_token
      ^ format ~parentheses ~stylizer subtree
      ^ Token.format ~stylizer closing_token
    | Block subtrees ->
      subtrees |> List.map (format ~parentheses ~stylizer) |> String.concat ""
  ;;
end

module Element = struct
  (** Represents a formatting element. *)
  type t =
    | Singleton of Token.t
    | Blob of Token.t list
    | Cluster of t list
    | Indented of int * t
    | Parenthesized of pair * t
    | Intercalated of Token.t list * t

  (** Represents a pair of characters (e.g. parentheses). *)
  and pair =
    | Parentheses
    | Brackets
    | Braces
    | Custom_pair of string * string

  (** [singleton token] creates a singleton element of [token]. *)
  let singleton = fun token -> Singleton token

  (** [blob tokens] groups [tokens] into an element. *)
  let blob = fun tokens -> Blob tokens

  (** [cluster elements] groups [elements] together. *)
  let cluster = fun elements -> Cluster elements

  (** [indented ~indent_count element] produces a new element
      that will be indented [indent_count] times when formatting
      it. Returns [None] if [indent_count] is negative. *)
  let indented =
    fun ~indent_count:count element ->
    if count < 0 then None else Some (Indented (count, element))
  ;;

  (** [indented_exn ~indent_count element] produces a new
      element that will be indented [indent_count] times when
      formatting it. Raises [Invalid_arg] if [indent_count] is
      negative. *)
  let indented_exn =
    fun ~indent_count:count element ->
    match indented ~indent_count:count element with
    | None -> invalid_arg "arg indent_count must be non-negative"
    | Some element -> element
  ;;

  (** [parenthsized ~pair element] produces a new element that
      will be surrounded by a [pair] when formatting it. *)
  let parenthesized = fun ~pair element -> Parenthesized (pair, element)

  (** [intercalated ~separating elements] produces a new element
      where the [separating] list of tokens is intercalated
      between the [elments]. *)
  let intercalated = fun ~separating elements -> Intercalated (separating, elements)

  (** [sequence ~pair elements] produces a new element where the
      [elements] are separated by a comma (and a space) and
      surrounded by [pair]. *)
  let sequence =
    fun ~pair elements ->
    elements
    |> intercalated ~separating:[ Token.comma; Token.space ]
    |> parenthesized ~pair
  ;;

  (** [lines elements] produces a new element that intersperses
      a line break between [elements] when formatting it,
      WITHOUT a trailing newline. *)
  let lines = fun elements -> intercalated ~separating:[ Token.line_break ] elements

  (** [format ~stylizer element] renders the [element] into a
      string using the [stylizer]. *)
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
      | Blob tokens -> tokens |> List.map (Token.format ~stylizer)
      | Cluster elements -> elements |> List.map (format_blocks ~stylizer) |> List.flatten
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
end

module type TOKENIZABLE = sig
  (** [TOKENIZABLE] is the interface for languages which terms
      can be transformed into a stream of formatter tokens. *)

  (** [t] encodes the language. *)
  type t

  (** [tokenize term] transforms [term] into a stream of
      formatter tokens. *)
  val tokenize : t -> Tree.t
end

module type CONVERTIBLE = sig
  (** [CONVERTIBLE] is the interface for types that can be
      transformed into a formatting element. *)

  type t

  val to_element : t -> Element.t
end

module Util = struct
  (** [tokenize value ~using:(module M)] transforms [value] to a
    list of tokens. *)
  let tokenize : type t. t -> using:(module TOKENIZABLE with type t = t) -> Tree.t =
    fun value ~using:(module M) -> M.tokenize value
  ;;

  (** [format ?stylizer value ~using:(module M)] transforms the
    [value] into a pretty-printable string using the [stylizer]
    if [M] provides tokenization for the [value] type. *)
  let format
    : type t.
      ?stylizer:Stylizer.t
      -> ?parentheses:string * string
      -> t
      -> using:(module TOKENIZABLE with type t = t)
      -> string
    =
    fun ?stylizer ?parentheses value ~using:(module M) ->
    (* TODO: handle line breaks / width-aware formatting *)
    value |> tokenize ~using:(module M) |> Tree.format ?parentheses ?stylizer
  ;;

  let format_via_element
    : type t.
      ?stylizer:Stylizer.t -> t -> using:(module CONVERTIBLE with type t = t) -> string
    =
    fun ?stylizer value ~using:(module M) -> Element.format ?stylizer (M.to_element value)
  ;;
end
