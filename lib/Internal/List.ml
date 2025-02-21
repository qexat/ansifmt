include Stdlib.List

(** [singleton element] creates a list with a single [element]. *)
let singleton : 'a -> 'a t = fun element -> [ element ]

(** [intersperse item list] inserts [item] between every
      element of [list]. This function is tail-recursive. *)
let[@tail_mod_cons] rec intersperse : 'a -> 'a t -> 'a t =
  fun item list ->
  match list with
  | [] -> []
  | [ last ] -> [ last ]
  | first :: rest -> first :: item :: intersperse item rest
;;
