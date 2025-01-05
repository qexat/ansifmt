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
    fun (token_type, lexeme) ->
    Printf.sprintf
      "%s%s\x1b[22;23;24;25;39;49m"
      (Styling.to_ansi (stylizer token_type))
      lexeme
  ;;
end

module type TOKENIZABLE = sig
  (** [TOKENIZABLE] is the interface for languages which terms
      can be transformed into a stream of formatter tokens. *)

  (** [t] encodes the language. *)
  type t

  (** [tokenize term] transforms [term] into a stream of
      formatter tokens. *)
  val tokenize : t -> Token.t list
end

module Util = struct
  (** [tokenize value ~using:(module M)] transforms [value] to a
    list of tokens. *)
  let tokenize : type t. t -> using:(module TOKENIZABLE with type t = t) -> Token.t list =
    fun value ~using:(module M) -> M.tokenize value
  ;;

  (** [format ?stylizer value ~using:(module M)] transforms the
    [value] into a pretty-printable string using the [stylizer]
    if [M] provides tokenization for the [value] type. *)
  let format
    : type t.
      ?stylizer:Stylizer.t -> t -> using:(module TOKENIZABLE with type t = t) -> string
    =
    fun ?(stylizer = Stylizer.default) value ~using:(module M) ->
    (* TODO: handle line breaks / width-aware formatting *)
    value
    |> tokenize ~using:(module M)
    |> List.map (Token.format ~stylizer)
    |> String.concat ""
  ;;
end
