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

module type TOKENIZABLE = sig
  (** [TOKENIZABLE] is the interface for languages which terms
      can be transformed into a stream of formatter tokens. *)

  (** [t] encodes the language. *)
  type t

  (** [tokenize term] transforms [term] into a stream of
      formatter tokens. *)
  val tokenize : t -> Tree.t
end

module Util = struct
  (** [tokenize value ~using:(module M)] transforms [value] to a
    list of tokens. *)
  let tokenize : type t. t -> using:(module TOKENIZABLE with type t = t) -> Tree.t =
    fun value ~using:(module M) -> M.tokenize value
  ;;

  let parenthesize_if
    : type t. (t -> bool) -> t -> using:(module TOKENIZABLE with type t = t) -> Tree.t
    =
    fun predicate value ~using:(module M) ->
    let base = tokenize value ~using:(module M) in
    if predicate value then Tree.parenthesized base else base
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
end
