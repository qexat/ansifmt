type t =
  { code : int
  ; parameters : int list
  }

let create ?(parameters = []) (code : int) : t =
  { code; parameters }
;;

let compose (left : t) (right : t) : t =
  let { code = code_left; parameters = parameters_left } : t =
    left
  in
  let { code = code_right; parameters = parameters_right } =
    right
  in
  { code = code_left
  ; parameters =
      parameters_left @ (code_right :: parameters_right)
  }
;;

let serialize ({ code; parameters } : t) : string =
  Printf.sprintf
    "{ code = %s ; parameters = %s }"
    (Int.to_string code)
    (parameters
     |> List.map Int.to_string
     |> String.concat "; "
     |> Printf.sprintf "[%s]")
;;

let show ({ code; parameters } : t) : string =
  code :: parameters
  |> List.map Int.to_string
  |> String.concat ";"
;;

let ( & ) = compose
