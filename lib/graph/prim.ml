module type WeightedUndirectedGraph = sig
  module Weight : sig
    type t
    val zero : t
    val compare : t -> t -> int
  end
  module rec Vertex : sig
    type t
    type set
    val universe : set
    val iter_connected_edges : t -> (Edge.t -> unit) -> unit
  end
  and Edge : sig
    type t
    val weight : t -> Weight.t
    val endpoint : t -> Vertex.t
  end
end

module type UnrootedWeightedTree = sig
  module Weight : sig
    type t
    val zero : t
    val compare : t -> t -> int
  end
  module Vertex : sig
    type t
    type set
    val universe : set
  end
  module Edge : sig
    type t
    val weight : t -> Weight.t
    val endpoint : t -> Vertex.t
    val fold_universe : (t -> 'a -> 'a) -> 'a -> 'a
  end
end

module F
  (Array : sig
    type t
    type key
    type elt
    type size
    val make : size -> t
    val get : t -> key -> elt
    val set : t -> key -> elt -> unit
  end)
  (Heap : sig
    type t
    type size = Array.size
    type elt
    type key = Array.elt
    val make : size -> t
    val add : t -> key -> elt -> unit
    val take_min_binding : t -> (key * elt) option
  end)
= struct
  type edge = Heap.elt
  type vertex = Array.key
  type weight = Array.elt
  type vertices = Array.size

  let minimum_spanning_tree
    (module G : WeightedUndirectedGraph
      with type Edge.t = edge
       and type Weight.t = weight
       and type Vertex.t = vertex
       and type Vertex.set = vertices) s =
    (module struct
      module Weight = G.Weight
      module Vertex = G.Vertex
      module Edge = struct
        include G.Edge
        let fold_universe f acc =
          let q = Heap.make G.Vertex.universe in
          let d = Array.make G.Vertex.universe in
          let rec prim acc u =
            Array.set d u G.Weight.zero;
            G.Vertex.iter_connected_edges u (fun e ->
              let w = G.Edge.weight e in
              let v = G.Edge.endpoint e in
              if 0 < Weight.compare (Array.get d v) w then
                (Array.set d v w; Heap.add q w e));
            prim' acc
          and prim' acc =
            match Heap.take_min_binding q with
            | None -> acc
            | Some (w, e) ->
                let u = G.Edge.endpoint e in
                if 0 < Weight.compare w (Array.get d u)
                then prim' acc
                else prim (f e acc) u in
          prim acc s
      end
    end : UnrootedWeightedTree
      with type Edge.t = edge
       and type Weight.t = weight
       and type Vertex.t = vertex
       and type Vertex.set = vertices)
end
