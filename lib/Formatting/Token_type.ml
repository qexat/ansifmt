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
  | Custom of Styling.t (* for e.g. non-semantical text *)
