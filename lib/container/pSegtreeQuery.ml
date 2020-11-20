(* セグ木のクエリについての実装
   これを直接使わず， PSegtree.F または PLazySegtree.F を用いること *)
module F
  (S : sig
    type t
    val op : t -> t -> t
  end)
  (Node : sig
    type t
    type elt = S.t
    val size : t -> int
    val case : t -> (elt -> 'a) -> (int -> elt -> t -> t -> 'a) -> 'a
  end)
= struct
  let rec get i t =
    Node.case t Fun.id (fun _ _ l r ->
      let lsize = Node.size l in
      if i < lsize
      then get i l
      else get (i - lsize) r)
  let get i t =
    assert (0 <= i && i < Node.size t);
    get i t

  let rec query l r t =
    Node.case t Fun.id
      (fun n data left right ->
        if l <= 0 && n <= r
        then data
        else
          let lsize = Node.size left in
          if r <= lsize
          then query l r left
          else if lsize <= l
          then query (l - lsize) (r - lsize) right
          else S.op (query l lsize left) (query 0 (r - lsize) right))
  let query l r t =
    assert (0 <= l && l < r && r <= Node.size t);
    query l r t
end
