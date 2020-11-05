type t

val empty_set : t
val empty_str : t
val char : Char.t -> t
val app : t -> t -> t
val neg : t -> t
val inter : t -> t -> t
val union : t -> t -> t
val star : t -> t

val matches : t -> Char.t list -> bool
