(* 普通のセグ木と遅延セグ木に共通する処理
   これを直接使わず， PSegtree.F または PLazySegtree.F を用いること *)
module F
  (S : sig
    type t
    val op : t -> t -> t
  end)
  (Node : sig
    type t
    type elt = S.t
    val leaf : elt -> t
    val make : t -> t -> t
    val case : t -> (elt -> 'a) -> (elt -> t -> t -> 'a) -> 'a
  end)
= struct
  type elt = S.t
  type t = { size : int; node : Node.t }

  (* セグ木の要素をどのように左右に分配するか
     正整数nについて lsize n + rsize n = n が成り立たなくてはならない *)
  let lsize n = n lsr 1
  let rsize n = (n + 1) lsr 1

  let rec of_list l = function
    | 1 -> Node.leaf (List.hd l), List.tl l
    | n ->
        let t1, l = of_list l (lsize n) in
        let t2, l = of_list l (rsize n) in
        Node.make t1 t2, l
  let of_list l =
    let n = List.length l in
    assert (0 < n);
    { size = n; node = fst (of_list l n) }

  let rec init i f = function
    | 1 -> Node.leaf (f i)
    | n -> Node.make (init i f (lsize n)) (init (i + lsize n) f (rsize n))
  let init n f =
    assert (1 <= n);
    { size = n; node = init 0 f n }

  let rec get n i t =
    Node.case t Fun.id (fun _ l r ->
      if i < lsize n
      then get (lsize n) i l
      else get (rsize n) (i - lsize n) r)
  let get i t =
    assert (0 <= i && i < t.size);
    get t.size i t.node

  let rec query n l r t =
    Node.case t
      (fun data -> data)
      (fun data left right ->
        if l <= 0 && n <= r
        then data
        else if r <= lsize n
        then query (lsize n) l r left
        else if lsize n <= l
        then query (rsize n) (l - lsize n) (r - lsize n) right
        else S.op (query (lsize n) l (lsize n) left) (query (rsize n) 0 (r - lsize n) right))
  let query l r t =
    assert (0 <= l && l < r && r <= t.size);
    query t.size l r t.node

  let rec update n i f t =
    Node.case t
      (fun data -> Node.leaf (f data))
      (fun _ left right ->
        if i < lsize n
        then Node.make (update (lsize n) i f left) right
        else Node.make left (update (rsize n) (i - lsize n) f right))
  let update i f t =
    assert (0 <= i && i < t.size);
    { t with node = update t.size i f t.node }

  let set i x = update i (Fun.const x)
end
