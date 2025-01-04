module Int8 : sig
  (** Represents an 8-bit integer. *)

  type t = private int

  (** [to_int value] converts [value] to a built-in integer. *)
  val to_int : t -> int

  (** [of_int value] converts [value] to an 8-bit integer.
      Returns [None] if [value] does not fit in 8 bits. *)
  val of_int : int -> t option

  (** [of_int_exn value] converts [value] to a 8-bit integer.
      Raises a [Failure] exception if [value] does not fit in 8
      bits. *)
  val of_int_exn : int -> t
end = struct
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
end

module Triplet = struct
  type ('a, 'b, 'c) t = 'a * 'b * 'c

  (** [all triplet] returns a version of [triplet] wrapped in
      [option], which is of the [Some] variant if every member
      is also of the [Some variant], [None] otherwise. *)
  let all : ('a option, 'b option, 'c option) t -> ('a, 'b, 'c) t option = function
    | Some first, Some second, Some third -> Some (first, second, third)
    | _ -> None
  ;;

  (** [all_ok triplet] returns a version of [triplet] wrapped in
      [result], which is of the [Ok] variant if every member
      is also of the [Ok] variant, otherwise it is whichever
      is the first [Error]. *)
  let all_ok
    : (('a, 'e) result, ('b, 'e) result, ('c, 'e) result) t -> (('a, 'b, 'c) t, 'e) result
    = function
    | Ok first, Ok second, Ok third -> Ok (first, second, third)
    | (Error _ as error), _, _ -> error
    | _, (Error _ as error), _ -> error
    | _, _, (Error _ as error) -> error
  ;;

  (** [all_error triplet] returns a version of [triplet] wrapped in 
      [result], which is of the [Error] variant if every member
      is also of the [Error] variant, otherwise it is whichever
      is the first [Ok]. *)
  let all_error (* Somehow ocamlformat leaves two spaces after the colon *)
    :  (('r, 'e1) result, ('r, 'e2) result, ('r, 'e3) result) t
    -> ('r, ('e1, 'e2, 'e3) t) result
    = function
    | Error first, Error second, Error third -> Error (first, second, third)
    | (Ok _ as ok), _, _ -> ok
    | _, (Ok _ as ok), _ -> ok
    | _, _, (Ok _ as ok) -> ok
  ;;

  (** [any_ok triplet] returns a version of [triplet] wrapped
      in [result], which is the first [Ok] encountered if there
      is any, otherwise it is the triplet wrapped in [Error].
      
      It is the same as [all_error], but it is semantically
      useful to have it as a separate function. *)
  let any_ok = all_error

  (** [map func1 func2 func3 triplet] produces a new triplet
      where each member is mapped to its corresponding function.
      That is, if [triplet] is [(first, second, third)], the
      result will be [(func1 first, func2 second, func3 third)]. *)
  let map (func1 : 'a -> 'd) (func2 : 'b -> 'e) (func3 : 'c -> 'f)
    : ('a, 'b, 'c) t -> ('d, 'e, 'f) t
    = function
    | first, second, third -> func1 first, func2 second, func3 third
  ;;

  (** [map_uniform func triplet] maps [func] to every member of
      the triplet.
      It is equivalent to [map func func func triplet]. *)
  let map_uniform ~(func : 'a -> 'b) : ('a, 'a, 'a) t -> ('b, 'b, 'b) t =
    map func func func
  ;;
end
