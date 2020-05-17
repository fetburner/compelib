module type Monoid = sig
  type t
  val e : t
  val op : t -> t -> t
end

module Make (S : Monoid) : sig
  type t
  type elt
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
end with type elt = S.t = struct
  type elt = S.t
  (* sizeがセグ木の要素数，dataは要素すべてをモノイドの演算で畳み込んだもの *)
  type t = { size : int; data : elt; body : body lazy_t }
  and body = Leaf | Node of t * t

  let make_node left right =
    { size = left.size + right.size;
      data = S.op left.data right.data;
      body = lazy (Node (left, right)) }

  let rec make n = 
    assert (1 <= n);
    { size = n;
      data = S.e;
      body = lazy
        begin match n with
        | 1 -> Leaf
        | n -> Node (make (n / 2), make ((n + 1) / 2))
        end }

  let rec update i f t =
    assert (0 <= i && i < t.size);
    match t with
    | { body = lazy Leaf } -> { t with data = f t.data }
    | { body = lazy (Node (left, right)) } ->
        if i < left.size then make_node (update i f left) right
        else make_node left (update (i - left.size) f right)

  let rec update_range l r f t =
    assert (0 <= l && l < r && r <= t.size);
    match t with
    | { body = lazy Leaf } -> { t with data = f t.data }
    | { body = lazy (Node (left, right)) } ->
        if l <= 0 && t.size <= r then
          { t with
            data = f t.data;
            body = lazy
              (Node
                (update_range 0 left.size f left,
                 update_range 0 right.size f right)) }
        else if r <= left.size then
          make_node (update_range l r f left) right
        else if left.size <= l then
          make_node left (update_range (l - left.size) (r - left.size) f right)
        else
          make_node
            (update_range l left.size f left)
            (update_range 0 (r - left.size) f right)

  let rec query l r t =
    assert (0 <= l && l < r && r <= t.size);
    match t with
    | { body = lazy Leaf } -> t.data
    | { body = lazy (Node (left, right)) } ->
        if l <= 0 && t.size <= r then t.data
        else if r <= left.size then query l r left
        else if left.size <= l then query (l - left.size) (r - left.size) right
        else S.op (query l left.size left) (query 0 (r - left.size) right)
end