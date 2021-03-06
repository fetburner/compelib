module F
  (S : sig
    type t
    val op : t -> t -> t
  end)
: sig
  type t
  type elt = S.t

  (* 与えられたリストの要素からなるセグ木を作る *)
  val of_list : elt list -> t
  (* f 0, ... f (n - 1)のn要素からなるセグ木を作る *)
  val init : int -> (int -> elt) -> t
  (* [get i t]: i番目の要素を返す *)
  val get : int -> t -> elt
  (* [query l r t]: 添字が[l, r)の要素を半群の演算子で畳み込んだ値を求める *)
  val query : int -> int -> t -> elt
  (* [set i x t]: i番目の要素をxに変更したセグ木を作る *)
  val set : int -> elt -> t -> t
  (* [update i f t]: i番目の要素x_iをf x_iに変更したセグ木を作る *)
  val update : int -> (elt -> elt) -> t -> t
end
