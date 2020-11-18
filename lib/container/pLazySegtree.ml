module F
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
  type map = F.t

  (* 遅延セグ木の遅延評価に関する処理を隠蔽するため，モジュールで包んでおく *)
  module Node
  : sig
    type t
    type elt = S.t
    val leaf : elt -> t
    val make : t -> t -> t
    val apply : map -> t -> t
    val case : t -> (elt -> 'a) -> (elt -> t -> t -> 'a) -> 'a
  end
  = struct
    (* data は要素すべてをモノイドの演算で畳み込んだもの
       pending は子孫のノードに適用されていない写像
       mutable だけど遅延していた計算結果の保存にしか使わないこと *)
    type elt = S.t
    type t =
      | Leaf of { mutable data : elt }
      | Node of { mutable data : elt; mutable pending : map; left : t; right : t }

    let data = function
      | Leaf { data = x } -> x
      | Node { data = x; _ } -> x

    let leaf data = Leaf { data }
    let make left right =
      Node { data = S.op (data left) (data right); pending = F.id; left; right }

    let propagate f = function
      | Leaf r -> r.data <- F.apply f r.data
      | Node r -> r.data <- F.apply f r.data; r.pending <- F.comp f r.pending

    let apply f = function
      | Leaf { data } -> Leaf { data = F.apply f data }
      | Node ({ data; pending; _ } as r) -> Node { r with data = F.apply f data; pending = F.comp f pending }

    let case t leaf node =
      match t with
      | Leaf { data } -> leaf data
      | Node ({ data; pending; left; right } as r) ->
          propagate pending left;
          propagate pending right;
          r.pending <- F.id;
          node data left right
  end

  include (PSegtreeCommon.F (S) (Node))

  let rec update_range n l r f t =
    Node.case t
      (fun data -> Node.leaf @@ F.apply f data)
      (fun _ left right ->
        if l <= 0 && n <= r
        then Node.apply f t
        else if r <= lsize n
        then Node.make (update_range (lsize n) l r f left) right
        else if lsize n <= l
        then Node.make left (update_range (rsize n) (l - lsize n) (r - lsize n) f right)
        else Node.make
          (update_range (lsize n) l (lsize n) f left)
          (update_range (rsize n) 0 (r - lsize n) f right))
  let update_range l r f t =
    assert (0 <= l && l < r && r <= t.size);
    { t with node = update_range t.size l r f t.node }
end
