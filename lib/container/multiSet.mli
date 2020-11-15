module type OrderedType =
  sig
    type t
    val compare: t -> t -> int
  end

module Make (Ord : OrderedType) : sig
  type elt = Ord.t
  type t
  (* 空集合 *)
  val empty : t
  (* 要素数 *)
  val cardinal : t -> int
  (* 要素xをn個追加 *)
  val add : elt -> int -> t -> t
  (* 要素xをn個削除 要素がn個以上存在しない場合は全て削除 *)
  val remove : elt -> int -> t -> t
  (* 要素xの数を数える *)
  val count : elt -> t -> int
  (* xより小さい要素の数を数える *)
  val count_lt : elt -> t -> int
  (* xより大きい要素の数を数える *)
  val count_gt : elt -> t -> int
  (* 昇順に見た時のn番目の要素 範囲外ならNot_foundを投げる *)
  val nth_inc : int -> t -> elt
  (* 降順に見た時のn番目の要素 範囲外ならNot_foundを投げる *)
  val nth_dec : int -> t -> elt
  (* 昇順に見てn要素を取り出す n要素に満たなければ全ての要素を昇順に列挙する *)
  val take_inc : int -> t -> elt list
  (* 降順に見てn要素を取り出す n要素に満たなければ全ての要素を降順に列挙する *)
  val take_dec : int -> t -> elt list
  (* 全ての要素を昇順に列挙する *)
  val elements_inc : t -> elt list
  (* 全ての要素を降順に列挙する *)
  val elements_dec : t -> elt list
  (* 昇順に要素を畳み込む *)
  val fold_inc : ('a -> elt -> 'a) -> 'a -> t -> 'a
  (* 降順に要素を畳み込む *)
  val fold_dec : (elt -> 'a -> 'a) -> t -> 'a -> 'a
  (* 最小の要素 空集合が与えられた場合はNot_foundを投げる *)
  val min_elt : t -> elt
  (* 最大の要素 空集合が与えられた場合はNot_foundを投げる *)
  val max_elt : t -> elt
end
