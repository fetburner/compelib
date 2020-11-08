module F
  (Weight : sig
    type t
    val zero : t
    val compare : t -> t -> int
  end)
  (Edge : sig
    type t
    type v
    type w = Weight.t
    val weight : t -> w
    val vertex : t -> v
  end)
  (Heap : sig
    type t
    type size
    type key = Edge.w
    type elt = Edge.t
    val make : size -> t
    val add : t -> key -> elt -> unit
    val take_min_binding : t -> (key * elt) option
  end)
  (Array : sig
    type t
    type key = Edge.v
    type elt = Edge.w
    type size = Heap.size
    val make : size -> t
    val get : t -> key -> elt
    val set : t -> key -> elt -> unit
  end)
= struct
  type edge = Edge.t
  type vertex = Edge.v
  type weight = Edge.w
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
