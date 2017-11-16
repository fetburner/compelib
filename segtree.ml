module type SemiGroup = sig
  type t
  val op : t -> t -> t
end

module Make (S : SemiGroup) : sig
  type t
  type elt
  (* f 0, ... f (n - 1)のn要素からなるセグ木を作る *)
  val init : int -> (int -> elt) -> t
  (* 
   * update i f t
   * i番目の要素x_iをf x_iに変更したセグ木を作る
   *)
  val update : int -> (elt -> elt) -> t -> t
  (*
   * find l r t
   * [l, r)の要素の積を求める
   *)
  val find : int -> int -> t -> elt
end with type elt = S.t = struct
  type elt = S.t
  type t =
    | Leaf of elt
    | Node of int * elt * t * t

  (* セグ木の要素数 *)
  let size = function
    | Leaf _ -> 1
    | Node (size, _, _, _) -> size

  (* セグ木の持っている要素全ての積 *)
  let product = function
    | Leaf product
    | Node (_, product, _, _) -> product

  let make_node l r =
    Node (size l + size r, S.op (product l) (product r), l, r)

  let rec init offset f = function
    | 1 -> Leaf (f offset)
    | n -> make_node (init offset f (n / 2)) (init (offset + n / 2) f ((n + 1) / 2))
  let init n f = init 0 f n

  let rec update i f t =
    assert (0 <= i && i < size t);
    match t with
    | Leaf x -> Leaf (f x)
    | Node (_, _, l, r) ->
        if i < size l then make_node (update i f l) r
        else make_node l (update (i - size l) f r)

  let rec find l r t =
    assert (0 <= l && l < r && r <= size t);
    match t with
    | Leaf x -> x
    | Node (_, _, left, right) ->
        if l = 0 && r = size t then product t
        else if r <= size left then find l r left
        else if size left <= l then find (l - size left) (r - size left) right
        else S.op (find l (size left) left) (find 0 (r - size left) right)
end
