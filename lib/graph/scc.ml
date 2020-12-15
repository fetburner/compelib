module type UnweightedDirectedGraph = sig
  module Vertex : sig
    type t
    type set
    val universe : set
    val fold_universe : (t -> 'a -> 'a) -> 'a -> 'a
    val fold_successors : t -> (t -> 'a -> 'a) -> 'a -> 'a
    val fold_predecessors : t -> (t -> 'a -> 'a) -> 'a -> 'a
  end
end

module type List = sig
  type t
  type elt
  val nil : t
  val cons : elt -> t -> t
end

module type Array = sig
  type t
  type elt
  type key
  type size
  val make : size -> t
  val get : t -> key -> elt
  val set : t -> key -> elt -> unit
end

module F
  (A : Array with type elt = bool)
  (L : List with type elt = A.key)
= struct
  type vertex = A.key
  type vertices = A.size

  let visit
    (module G : UnweightedDirectedGraph
      with type Vertex.t = vertex
      and type Vertex.set = vertices) vs =
    let rec visit v l =
      if A.get vs v
      then l
      else (A.set vs v true; L.cons v (G.Vertex.fold_successors v visit l)) in
    visit

  let sort
    (module G : UnweightedDirectedGraph
      with type Vertex.t = vertex
       and type Vertex.set = vertices) =
    let vs = A.make G.Vertex.universe in
    G.Vertex.fold_universe (visit (module G) vs) L.nil
end

module G
  (A : Array with type elt = bool)
  (L : List with type elt = A.key)
  (LL : List with type elt = L.t)
= struct
  type vertex = A.key
  type vertices = A.size

  let scc
    (module G : UnweightedDirectedGraph
      with type Vertex.t = vertex
       and type Vertex.set = vertices) =
    let vs = A.make G.Vertex.universe in
    let module G' = struct
      module Vertex = struct
        include G.Vertex
        let fold_successors = G.Vertex.fold_predecessors
        let fold_predecessors = G.Vertex.fold_successors
      end
    end in
    let module M = F (A) (L) in
    let module N = F (A)
      (struct
        type t = LL.t -> LL.t
        type elt = vertex
        let nil = Fun.id
        let cons v k l = k @@
          if A.get vs v
          then l
          else LL.cons (M.visit (module G) vs v L.nil) l
      end) in
    N.sort (module G') LL.nil
end
