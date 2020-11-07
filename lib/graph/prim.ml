module F
  (Tree : sig
    type t
    type v
    type e
    val empty : t
    val add : e -> t -> t
  end)
  (Weight : sig
    type t
    val zero : t
    val compare : t -> t -> int
  end)
  (Heap : sig
    type t
    type size
    type key = Weight.t
    type elt = Tree.v * Tree.e
    val make : size -> t
    val add : t -> key -> elt -> unit
    val take_min_binding : t -> (key * elt) option
  end)
  (Array : sig
    type t
    type key = Tree.v
    type elt = Heap.key
    type size = Heap.size
    val make : size -> t
    val get : t -> key -> elt
    val set : t -> key -> elt -> unit
  end)
= struct
  type edge = Tree.e
  type tree = Tree.t
  type vertex = Tree.v
  type weight = Weight.t
  type vertices = Array.size

  let minimum_spanning_tree n es s =
    let q = Heap.make n in
    let d = Array.make n in
    let rec prim t u =
      Array.set d u Weight.zero;
      es u (fun v w e ->
        if 0 < Weight.compare (Array.get d v) w then
          (Array.set d v w; Heap.add q w (v, e)));
      prim' t
    and prim' t =
      match Heap.take_min_binding q with
      | None -> t
      | Some (w, (u, e)) ->
          if 0 < Weight.compare w (Array.get d u)
          then prim' t
          else prim (Tree.add e t) u in
    prim Tree.empty s
end
