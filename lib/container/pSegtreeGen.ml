module F
  (Node : sig
    type t
    type elt
    val leaf : elt -> t
    val make : t -> t -> t

    (* セグ木の要素をどのように左右に分配するか
       正整数nについて lsize n + rsize n = n が成り立たなくてはならない *)
    val lsize : int -> int
    val rsize : int -> int
  end)
= struct
  let rec of_list l = function
    | 1 -> Node.leaf (List.hd l), List.tl l
    | n ->
        let t1, l = of_list l (Node.lsize n) in
        let t2, l = of_list l (Node.rsize n) in
        Node.make t1 t2, l
  let of_list l n =
    assert (0 < n);
    fst (of_list l n)

  let rec init i f = function
    | 1 -> Node.leaf (f i)
    | n ->
        let l = Node.lsize n in
        Node.make (init i f l) (init (i + l) f (Node.rsize n))
  let init n f =
    assert (1 <= n);
    init 0 f n
end
