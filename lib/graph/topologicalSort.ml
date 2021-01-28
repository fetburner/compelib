module type UnweightedDirectedGraph = sig
  module Vertex : sig
    type t
    type set
    val universe : set
    val fold_universe : (t -> 'a -> 'a) -> 'a -> 'a
    val fold_adjacencies : t -> (t -> 'a -> 'a) -> 'a -> 'a
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
      else (A.set vs v true; L.cons v (G.Vertex.fold_adjacencies v visit l)) in
    visit

  let sort
    (module G : UnweightedDirectedGraph
      with type Vertex.t = vertex
       and type Vertex.set = vertices) =
    let vs = A.make G.Vertex.universe in
    G.Vertex.fold_universe (visit (module G) vs) L.nil
end
