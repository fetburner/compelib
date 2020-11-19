module F
  (S : sig
    type t
    val op : t -> t -> t
  end)
= struct
  type elt = S.t

  let lsize n = n lsr 1
  let rsize n = (n + 1) lsr 1

  module Node = struct
    type t = Leaf of elt | Node of elt * t * t
    type nonrec elt = elt

    let data = function
      | Leaf x -> x
      | Node (x, _, _) -> x

    let leaf x = Leaf x
    let make l r = Node (S.op (data l) (data r), l, r)

    let case t leaf node =
      match t with
      | Leaf x -> leaf x
      | Node (x, l, r) -> node x l r

    let lsize = lsize
    let rsize = rsize
  end

  module Tree = struct
    type t = { size : int; node : Node.t }
    type nonrec elt = elt

    let size t = t.size
    let leaf x = { size = 1; node = Node.leaf x }
    let make l r = { size = l.size + r.size; node = Node.make l.node r.node }

    let case t leaf node =
      Node.case t.node leaf (fun x l r ->
        node t.size x
          { size = lsize t.size; node = l }
          { size = rsize t.size; node = r })
  end

  type t = Tree.t

  module Gen = PSegtreeGen.F (Node)

  let of_list l =
    let n = List.length l in
    { Tree.size = n; Tree.node = Gen.of_list l n }

  let init n f = { Tree.size = n; Tree.node = Gen.init n f }

  module Query = PSegtreeQuery.F (S) (Tree)
  include Query

  module Update = PSegtreeUpdate.F (Tree)
  include Update
end
