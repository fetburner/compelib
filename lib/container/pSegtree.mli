module type SemiGroup = sig
  type t
  val op : t -> t -> t
end

module F (S : SemiGroup) : sig
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

module LazyF
  (S : SemiGroup)
  (* 半群の自己同型写像 *)
  (F : sig
    type t
    type dom = S.t
    type cod = S.t
    (* 恒等写像 *)
    val id : t
    (* 写像の合成 *)
    val comp : t -> t -> t
    (* 半群の要素に写像を適用する *)
    val apply : t -> dom -> cod
  end)
: sig
  type t
  type elt = S.t
  type map = F.t

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
  (* [update_range l r f t]: [l, r) の要素に自己同型写像 f を適用したセグ木を作る *)
  val update_range : int -> int -> map -> t -> t
end
