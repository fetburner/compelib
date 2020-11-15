module type OrderedType = sig
  type t
  val compare : t -> t -> int
end

module Make (Key : OrderedType) (Priority : OrderedType) : sig
  type key = Key.t
  type priority = Priority.t

  type t
  val is_empty : t -> bool
  val cardinal : t -> int
  val find_opt : key -> t -> priority option
  val mem : key -> t -> bool
  val min_binding_opt : t -> (key * priority) option
  val empty : t
  val singleton : key -> priority -> t
  val add : key -> priority -> t -> t
  val update : key -> (priority option -> priority option) -> t -> t
  val remove : key -> t -> t
  val remove_min_binding : t -> t
  val of_list : (key * priority) list -> t
  val fold : (key -> priority -> 'a -> 'a) -> t -> 'a -> 'a
  val bindings : t -> (key * priority) list
end
