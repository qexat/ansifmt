include Stdlib.List

(** [intersperse item list] inserts [item] between every
      element of [list]. This function is tail-recursive. *)
let intersperse : 'a -> 'a t -> 'a t =
  let rec tailrec item list acc =
    match list with
    | [] -> rev acc
    | last :: [] -> tailrec item [] (last :: acc)
    | first :: rest -> tailrec item rest (item :: first :: acc)
  in
  fun item list -> tailrec item list []
;;
