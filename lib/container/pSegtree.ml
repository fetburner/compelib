module F
  (S : sig
    type t
    val op : t -> t -> t
  end)
= struct
  module Common = PSegtreeCommon.F (S)
    (struct
      type elt = S.t
      type t = Leaf of elt | Node of elt * t * t

      let data = function
        | Leaf x -> x
        | Node (x, _, _) -> x

      let leaf x = Leaf x
      let make l r = Node (S.op (data l) (data r), l, r)

      let case t leaf node =
        match t with
        | Leaf x -> leaf x
        | Node (x, l, r) -> node x l r
    end)
  include Common
end
