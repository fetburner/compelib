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

  (* data は要素すべてをモノイドの演算で畳み込んだもの
     pending は適用されていない写像
     mutable だけど遅延していた計算結果の保存にしか使わないこと *)
  type node = { mutable data : elt; mutable pending : map; child : (node * node) option }
  type t = { size : int; node : node }

  let make_node left right =
    { data = S.op left.data right.data;
      pending = F.id;
      child = Some (left, right) }

  (* セグ木の要素をどのように左右に分配するか
     正整数nについて lsize n + rsize n = n が成り立たなくてはならない *)
  let lsize n = n lsr 1
  let rsize n = (n + 1) lsr 1

  let rec of_list l = function
    | 1 -> { data = List.hd l; pending = F.id; child = None }, List.tl l
    | n ->
        let t1, l = of_list l (lsize n) in
        let t2, l = of_list l (rsize n) in
        make_node t1 t2, l
  let of_list l =
    let n = List.length l in
    assert (0 < n);
    { size = n; node = fst (of_list l n) }

  let rec init i f = function
    | 1 -> { data = f i; pending = F.id; child = None }
    | n -> make_node (init i f (lsize n)) (init (i + lsize n) f (rsize n))
  let init n f =
    assert (1 <= n);
    { size = n; node = init 0 f n }

  let force t =
    Option.iter (fun (t1, t2) ->
      t1.pending <- F.comp t.pending t1.pending;
      t2.pending <- F.comp t.pending t2.pending) t.child;
    t.data <- F.apply t.pending t.data;
    t.pending <- F.id

  let rec update n i f t =
    force t;
    match t.child with
    | None -> { t with data = f t.data }
    | Some (left, right) ->
        if i < lsize n
        then make_node (update (lsize n) i f left) right
        else make_node left (update (rsize n) (i - lsize n) f right)
  let update i f t =
    assert (0 <= i && i < t.size);
    { t with node = update t.size i f t.node }

  let rec update_range n l r f t =
    if l <= 0 && n <= r
    then { t with pending = F.comp f t.pending }
    else begin
      force t;
      match t.child with
      | None -> failwith "update_range"
      | Some (left, right) ->
          if r <= lsize n
          then make_node (update_range (lsize n) l r f left) right
          else if lsize n <= l
          then make_node left (update_range (rsize n) (l - lsize n) (r - lsize n) f right)
          else make_node
                 (update_range (lsize n) l (lsize n) f left)
                 (update_range (rsize n) 0 (r - lsize n) f right)
    end
  let update_range l r f t =
    assert (0 <= l && l < r && r <= t.size);
    { t with node = update_range t.size l r f t.node }

  let rec query n l r t =
    if l <= 0 && n <= r
    then t.data
    else begin
      force t;
      match t.child with
      | None -> failwith "query"
      | Some (left, right) ->
          if r <= lsize n
          then query (lsize n) l r left
          else if lsize n <= l
          then query (rsize n) (l - lsize n) (r - lsize n) right
          else S.op (query (lsize n) l (lsize n) left) (query (rsize n) 0 (r - lsize n) right)
    end
  let query l r t =
    assert (0 <= l && l < r && r <= t.size);
    query t.size l r t.node
end
