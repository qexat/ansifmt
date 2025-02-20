type t = int

let to_int : t -> int = fun value -> value

let of_int (value : int) : t option =
  if not (0 <= value && value < 256) then None else Some value
;;

let of_int_exn (value : int) : t =
  match of_int value with
  | None -> failwith "expected an integer between 0 and 255 (both included)"
  | Some value' -> value'
;;
