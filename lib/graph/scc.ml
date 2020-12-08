module type UnweightedDirectedGraph = sig
  module Vertex : sig
    type t
    type set
    val universe : set
    val universe_fold : (t -> 'a -> 'a) -> 'a -> 'a
    val fold_adjacencies : t -> (t -> 'a -> 'a) -> 'a -> 'a
  end
end

module type List = sig
  type t
  type elt
  val nil : t
  val cons : elt -> t -> t
end

module F
  (Array : sig
    type t
    type elt = bool
    type key
    type size
    val make : size -> t
    val get : t -> key -> elt
    val set : t -> key -> elt -> unit
  end)
= struct
  type vertex = Array.key
  type vertices = Array.size

  let visit (type l)
    (module L : List with type t = l and type elt = vertex)
    (module G : UnweightedDirectedGraph with type Vertex.t = vertex) vs =
    let rec visit v l =
      if Array.get vs v
      then l
      else (Array.set vs v true; L.cons v (G.Vertex.fold_adjacencies v visit l)) in
    visit

  let sort (type l)
    (module L : List with type t = l and type elt = vertex)
    (module G : UnweightedDirectedGraph
      with type Vertex.t = vertex
       and type Vertex.set = vertices) =
    let vs = Array.make G.Vertex.universe in
    G.Vertex.universe_fold (visit (module L) (module G) vs) L.nil

  let scc (type l) (type ll)
    (module L : List with type t = l and type elt = vertex)
    (module LL : List with type t = ll and type elt = l)
    (module G : UnweightedDirectedGraph
      with type Vertex.t = vertex
       and type Vertex.set = vertices) =
    let vs = Array.make G.Vertex.universe in
    sort
      (module struct
        include LL
        type elt = vertex
        let cons v l =
          if Array.get vs v
          then l
          else LL.cons (visit (module L) (module G) vs v L.nil) l
      end) (module G)
end
