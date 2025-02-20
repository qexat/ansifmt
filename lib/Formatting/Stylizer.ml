type t = Token_type.t -> Styling.t

let default : t =
  let open Token_type in
  function
  | Comment | Documentation -> Styling.create ~dim:true ()
  | Keyword | Punctuation_strong -> Styling.create ~foreground:Color.magenta ~bold:true ()
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
