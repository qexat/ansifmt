include Stdlib.Option

(** [last_some left right] returns the last [Some] of the two
    or [None] if neither of the two are. *)
let last_some : 'a t -> 'a t -> 'a t =
  fun left right ->
  match right with
  | Some _ -> right
  | None -> left
;;
