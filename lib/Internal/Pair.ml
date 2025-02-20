type ('a, 'b) t = 'a * 'b

(** [first (a, b)] is [a]. *)
let first : ('a, 'b) t -> 'a = fun (first, _) -> first

(** [second (a, b)] is [b]. *)
let second : ('a, 'b) t -> 'b = fun (_, second) -> second

(** [map func_first func_second (first, second)] applies each
    [func] to its respective item. *)
let map : ('a -> 'c) -> ('b -> 'd) -> ('a, 'b) t -> ('c, 'd) t =
  fun func_first func_second (first, second) -> func_first first, func_second second
;;

(** [map_first func (first, second)] is [(func first, second)]. *)
let map_first : ('a -> 'c) -> ('a, 'b) t -> ('c, 'b) t =
  fun func pair -> map func Fun.id pair
;;

(** [map_second func (first, second)] is [(first, func second)]. *)
let map_second : ('b -> 'c) -> ('a, 'b) t -> ('a, 'c) t =
  fun func pair -> map Fun.id func pair
;;

(** [map_uniform func pair] applies [func] to both elements of
    the pair. *)
let map_uniform : ('a -> 'b) -> ('a, 'a) t -> ('b, 'b) t =
  fun func pair -> map func func pair
;;
