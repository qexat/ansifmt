include Stdlib.List

(** [singleton element] creates a list with a single [element]. *)
let singleton : 'a -> 'a t = fun element -> [ element ]

(** [intersperse item list] inserts [item] between every
      element of [list]. This function is tail-recursive. *)
let intersperse : 'a -> 'a t -> 'a t =
  let[@tail_mod_cons] rec tailrec item list =
    match list with
    | [] -> []
    | [last] -> [last]
    | first :: rest -> first :: item :: tailrec item rest
  in
  fun item list -> tailrec item list []
;;
