module Make
  (S : sig
    type t
    val op : t -> t -> t
  end)
  (F : sig
    type t
    type dom = S.t
    type cod = S.t
    val id : t
    val comp : t -> t -> t
    val apply : t -> dom -> cod
  end)
= struct
  type elt = S.t
  type map = F.t

  (* 遅延セグ木の遅延評価に関する処理を隠蔽するため，モジュールで包んでおく *)
  module Node
  : sig
    type t
    type thunk
    val leaf : elt -> t
    val make : t -> t -> t
    val force : thunk -> t
    val apply : map -> t -> thunk
    val case : t -> (elt -> (thunk * thunk) option -> 'a) -> 'a
  end
  = struct
    (* data は要素すべてをモノイドの演算で畳み込んだもの
       pending は適用されていない写像
       mutable だけど遅延していた計算結果の保存にしか使わないこと *)
    type thunk = { mutable data : elt; mutable pending : map; child : (thunk * thunk) option }
    (* thunk のうち，pending が F.id のもの *)
    type t = thunk

    let leaf data = { data; pending = F.id; child = None }
    let make left right =
      { data = S.op left.data right.data;
        pending = F.id;
        child = Some (left, right) }

    let force t =
      Option.iter (fun (t1, t2) ->
        t1.pending <- F.comp t.pending t1.pending;
        t2.pending <- F.comp t.pending t2.pending) t.child;
      t.data <- F.apply t.pending t.data;
      t.pending <- F.id; t

    let apply f t = { t with pending = f }

    let case t k = k t.data t.child
  end

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

  let rec update n i f t =
    Node.case t @@ fun data -> function
      | None -> Node.leaf (f data)
      | Some (left, right) ->
          if i < lsize n
          then Node.make (update (lsize n) i f (Node.force left)) (Node.force right)
          else Node.make (Node.force left) (update (rsize n) (i - lsize n) f (Node.force right))
  let update i f t =
    assert (0 <= i && i < t.size);
    { t with node = update t.size i f t.node }

  let rec update_range n l r f t =
    Node.case t @@ fun data -> function
      | None -> Node.leaf (F.apply f data)
      | Some (left, right) ->
          if l <= 0 && n <= r
          then Node.force (Node.apply f t)
          else if r <= lsize n
          then Node.make (update_range (lsize n) l r f (Node.force left)) (Node.force right)
          else if lsize n <= l
          then Node.make (Node.force left) (update_range (rsize n) (l - lsize n) (r - lsize n) f (Node.force right))
          else Node.make
                 (update_range (lsize n) l (lsize n) f (Node.force left))
                 (update_range (rsize n) 0 (r - lsize n) f (Node.force right))
  let update_range l r f t =
    assert (0 <= l && l < r && r <= t.size);
    { t with node = update_range t.size l r f t.node }

  let rec query n l r t =
    Node.case t @@ fun data -> function
      | None -> data
      | Some (left, right) ->
          if l <= 0 && n <= r
          then data
          else if r <= lsize n
          then query (lsize n) l r (Node.force left)
          else if lsize n <= l
          then query (rsize n) (l - lsize n) (r - lsize n) (Node.force right)
          else S.op (query (lsize n) l (lsize n) (Node.force left)) (query (rsize n) 0 (r - lsize n) (Node.force right))
  let query l r t =
    assert (0 <= l && l < r && r <= t.size);
    query t.size l r t.node
end
