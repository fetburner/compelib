(* persistent union-find data structure *)
type t
type class_

val make : int -> t
val find : t -> int -> class_
val union : t -> int -> int -> t
