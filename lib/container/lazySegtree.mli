module Make
  (* 半群 *)
  (S : sig
    type t
    val op : t -> t -> t
  end)
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

  (* 与えられたリストの要素からなる遅延セグ木を作る *)
  val of_list : elt list -> t
  (* f 0, ... f (n - 1)のn要素からなる遅延セグ木を作る *)
  val init : int -> (int -> elt) -> t
  (* 
   * update i f t
   * i 番目の要素に f を適用したセグ木を作る
   *)
  val update : int -> (elt -> elt) -> t -> t
  (*
   * update_range l r f t
   * [l, r) の要素に自己同型写像 f を適用したセグ木を作る
   *)
  val update_range : int -> int -> map -> t -> t
  (*
   * query l r t
   * [l, r) の要素の積を求める
   *)
  val query : int -> int -> t -> elt
end
