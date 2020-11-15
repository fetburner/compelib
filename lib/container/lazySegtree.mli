module type Monoid = sig
  type t
  val e : t
  val op : t -> t -> t
end

module Make (S : Monoid) : sig
  type t
  type elt = S.t
  (* n要素の単位元からなるセグ木を作る *)
  val make : int -> t
  (* 
   * update i f t
   * i番目の要素にfを適用したセグ木を作る
   *)
  val update : int -> (elt -> elt) -> t -> t
  (*
   * update_range l r f t
   * [l, r)の要素に自己準同型写像fを適用したセグ木を作る
   *)
  val update_range : int -> int -> (elt -> elt) -> t -> t
  (*
   * query l r t
   * [l, r)の要素の積を求める
   *)
  val query : int -> int -> t -> elt
end
