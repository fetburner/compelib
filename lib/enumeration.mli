(* 与えられたリストからn要素を選ぶ順列をストリームとして列挙 *)
val perm : int -> 'a list -> 'a list Seq.t
(* 与えられたリストからn要素を選ぶ組み合わせをストリームとして列挙 *)
val comb : int -> 'a list -> 'a list Seq.t
(* 与えられたリストから重複を許してn要素を選ぶ組み合わせをストリームとして列挙 *)
val repcomb : int -> 'a list -> 'a list Seq.t
