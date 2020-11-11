module F
  (Weight : sig
    type t
    val zero : t
    val compare : t -> t -> int
  end)
  (Edge : sig
    type t
    type vertex
    type weight = Weight.t
    val weight : t -> weight
    val vertex : t -> vertex
  end)
  (Heap : sig
    type t
    type size
    type elt = Edge.t
    type key = Edge.weight
    val make : size -> t
    val add : t -> key -> elt -> unit
    val take_min_binding : t -> (key * elt) option
  end)
  (Array : sig
    type t
    type key = Edge.vertex
    type elt = Edge.weight
    type size = Heap.size
    val make : size -> t
    val get : t -> key -> elt
    val set : t -> key -> elt -> unit
  end)
= struct
  type edge = Edge.t
  type vertex = Edge.vertex
  type weight = Edge.weight
  type vertices = Array.size

  let minimum_spanning_tree n es s f acc =
    let q = Heap.make n in
    let d = Array.make n in
    let rec prim acc u =
      Array.set d u Weight.zero;
      es u (fun e ->
        let v = Edge.vertex e in
        let w = Edge.weight e in
        if 0 < Weight.compare (Array.get d v) w then
          (Array.set d v w; Heap.add q w e));
      prim' acc
    and prim' acc =
      match Heap.take_min_binding q with
      | None -> acc
      | Some (w, e) ->
          let u = Edge.vertex e in
          if 0 < Weight.compare w (Array.get d u)
          then prim' acc
          else prim (f e acc) u in
    prim acc s
end
