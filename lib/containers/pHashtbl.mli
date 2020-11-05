(* 永続ハッシュテーブル（は？） *)
type ('a, 'b) t

val create : ?random:bool -> int -> ('a, 'b) t
val add : ('a, 'b) t -> 'a -> 'b -> ('a, 'b) t
val remove : ('a, 'b) t -> 'a -> ('a, 'b) t
val replace : ('a, 'b) t -> 'a -> 'b -> ('a, 'b) t
val find : ('a, 'b) t -> 'a -> 'b
val iter : ('a -> 'b -> unit) -> ('a, 'b) t -> unit
val fold : ('a -> 'b -> 'c -> 'c) -> ('a, 'b) t -> 'c -> 'c
val length : ('a, 'b) t -> int
