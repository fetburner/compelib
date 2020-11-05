module type SemiGroup = sig
  type t
  val op : t -> t -> t
end

module Make (S : SemiGroup) : sig
  type t
  type elt

  (* 与えられたリストの要素からなるセグ木を作る *)
  val of_list : elt list -> t
  (* f 0, ... f (n - 1)のn要素からなるセグ木を作る *)
  val init : int -> (int -> elt) -> t
  (*
   * update i f t
   * i番目の要素x_iをf x_iに変更したセグ木を作る
   *)
  val update : int -> (elt -> elt) -> t -> t
  (*
   * query l r t
   * 添字が[l, r)の要素を半群の演算子で畳み込んだ値を求める
   *)
  val query : int -> int -> t -> elt
end with type elt = S.t = struct
  type elt = S.t

  type body =
    | Leaf of elt
    | Node of elt * body * body
  type t = { size : int; body : body }

  (* セグ木の要素をどのように左右に分配するか
     正整数nについて lsize n + rsize n = n が成り立たなくてはならない *)
  let lsize n = n lsr 1
  let rsize n = (n + 1) lsr 1

  (* セグ木の保持する要素を半群の演算子で畳み込んだもの *)
  let data = function
    | Leaf x
    | Node (x, _, _) -> x

  let mknode l r = Node (S.op (data l) (data r), l, r)

  let rec of_list l = function
    | 1 -> Leaf (List.hd l), List.tl l
    | n ->
        let t1, l = of_list l (lsize n) in
        let t2, l = of_list l (rsize n) in
        mknode t1 t2, l
  let of_list l =
    let n = List.length l in
    assert (0 < n);
    { size = n; body = fst (of_list l n) }

  let rec init i f = function
    | 1 -> Leaf (f i)
    | n -> mknode (init i f (lsize n)) (init (i + lsize n) f (rsize n))
  let init n f =
    assert (1 <= n);
    { size = n; body = init 0 f n }

  let rec update n i f = function
    | Leaf x -> Leaf (f x)
    | Node (_, l, r) ->
        if i < lsize n
        then mknode (update (lsize n) i f l) r
        else mknode l (update (rsize n) (i - lsize n) f r)
  let update i f t =
    assert (0 <= i && i < t.size);
    { t with body = update t.size i f t.body }

  let rec query n l r = function
    | Leaf x -> x
    | Node (x, left, right) ->
        if l = 0 && r = n then x
        else if r <= lsize n then query (lsize n) l r left
        else if lsize n <= l then query (rsize n) (l - lsize n) (r - lsize n) right
        else S.op (query (lsize n) l (lsize n) left) (query (rsize n) 0 (r - lsize n) right)
  let query l r t =
    assert (0 <= l && l < r && r <= t.size);
    query t.size l r t.body
end
