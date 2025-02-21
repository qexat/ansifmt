module type TO_ELEMENT = sig
  (** [TO_ELEMENT] is the interface for types that can be
      transformed into a formatting element. *)

  type t

  val to_element : t -> Element.t
end
