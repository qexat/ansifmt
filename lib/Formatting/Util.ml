(** [to_element value (module M)] converts [value] to an element
    using the transforming function provided by [M]. *)
let to_element
  : type t. t -> using:(module Interfaces.TO_ELEMENT with type t = t) -> Element.t
  =
  fun value ~using:(module M) -> M.to_element value
;;

(** [format ?stylizer value (module M)] renders [value] to a
      string provided that it can be converted to an element which
      transforming function is given in [M].*)
let format
  : type t.
    ?stylizer:Stylizer.t
    -> t
    -> using:(module Interfaces.TO_ELEMENT with type t = t)
    -> string
  =
  fun ?stylizer value ~using -> value |> to_element ~using |> Element.format ?stylizer
;;
