module type CONVERTIBLE = sig
  (** [CONVERTIBLE] is the interface for types that can be
      transformed into a formatting element. *)

  type t

  val to_element : t -> Element.t
end
