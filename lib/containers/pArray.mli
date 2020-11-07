(* persistent array *)
type 'a t

val init : int -> (int -> 'a) -> 'a t
val make : int -> 'a -> 'a t
val of_list : 'a list -> 'a t
val get : 'a t -> int -> 'a
val set : 'a t -> int -> 'a -> 'a t
(* [set a n x] returns a persistent array replacing element number [n] of [a] with [x].
   [a] is not modified. *)
val length : 'a t -> int
val to_list : 'a t -> 'a list
val iter : ('a -> unit) -> 'a t -> unit
val iteri : (int -> 'a -> unit) -> 'a t -> unit
val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a
val fold_right : ('b -> 'a -> 'a) -> 'b t -> 'a -> 'a
