(** [Token] is the formatter token data type. *)

(** A token is simply a pair (token type, lexeme). *)
type t = Token_type.t * string

(** Token representing a single whitespace. *)
let space : t = Token_type.Space, " "

(** Token representing a line break.
  It is useful if you want to have a potential newline if
  it does not fit in one line. *)
let line_break : t = Token_type.Line_break, "\n"

(** Token representing a comma. *)
let comma : t = Token_type.Punctuation_weak, ","

(** Token representing a colon. *)
let colon : t = Token_type.Punctuation_strong, ":"

(** [number lexeme] constructs a number literal token. *)
let number (lexeme : string) : t = Token_type.Literal_constant, lexeme

let format ?(stylizer : Stylizer.t = Stylizer.default) : t -> string =
  fun (token_type, lexeme) -> Styling.wrap ~contents:lexeme (stylizer token_type)
;;
