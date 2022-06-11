module type Monoid = sig
  type t

  val e : t
  val op : t -> t -> t
end

module Make (M : Monoid) : sig
  type t
  type elt = M.t

  (* 与えられたリストの要素からなるセグ木を作る *)
  val of_list : elt list -> t
  (* f 0, ... f (n - 1)のn要素からなるセグ木を作る *)
  val init : int -> (int -> elt) -> t
  (* [get t i]: i番目の要素を返す *)
  val get : t -> int -> elt
  (* [product t l r]: 添字が[l, r)の要素を半群の演算子で畳み込んだ値を求める *)
  val product : t -> int -> int -> elt
  (* [upper_bound t l p]: [l, r) の区間積が述語 p を満たすような最大の右端 r と区間積を返す *)
  val upper_bound : t -> int -> (elt -> bool) -> int * elt
  (* [lower_bound t r p]: [l, r) の区間積が述語 p を満たすような最大の左端 l と区間積を返す *)
  val lower_bound : t -> int -> (elt -> bool) -> int * elt
  (* [set t i x]: i番目の要素をxに破壊的に変更する *)
  val set : t -> int -> elt -> unit
  (* [update t i f]: i番目の要素x_iをf x_iに破壊的に変更する *)
  val update : t -> int -> (elt -> elt) -> unit
end
