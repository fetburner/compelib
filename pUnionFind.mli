(* persistent union find *)
type t

val make : int -> t
val find : t -> int -> int
val union : t -> int -> int -> t
