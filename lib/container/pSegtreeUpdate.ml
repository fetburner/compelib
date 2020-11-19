(* 永続セグ木の更新についての実装
   これを直接使わず， PSegtree.F または PLazySegtree.F を用いること *)
module F
  (Node : sig
    type t
    type elt
    val size : t -> int
    val leaf : elt -> t
    val make : t -> t -> t
    val case : t -> (elt -> 'a) -> (int -> elt -> t -> t -> 'a) -> 'a
  end)
= struct
  let rec update i f t =
    Node.case t
      (fun data -> Node.leaf (f data))
      (fun _ _ left right ->
        let lsize = Node.size left in
        if i < lsize
        then Node.make (update i f left) right
        else Node.make left (update (i - lsize) f right))
  let update i f t =
    assert (0 <= i && i < Node.size t);
    update i f t

  let set i x = update i (Fun.const x)
end
