module type UnweightedDirectedGraph = sig
  module Vertex : sig
    type t
    type set
    val universe : set
    val eq : t -> t -> bool
    val iter_universe : (t -> unit) -> unit
    val iter_adjacencies : t -> (t -> unit) -> unit
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
  (A : Array with type elt = int)
  (L : List with type elt = A.key)
  (LL : List with type elt = L.t)
= struct
  type vertex = A.key
  type vertices = A.size

  let scc
    (module G : UnweightedDirectedGraph
      with type Vertex.t = vertex
       and type Vertex.set = vertices) =
    let tm = ref 0 in
    let scc = ref LL.nil in
    let st = Stack.create () in
    let cmp = A.make G.Vertex.universe in
    let ord = A.make G.Vertex.universe in
    let low = A.make G.Vertex.universe in
    let rec visit u =
      incr tm;
      A.set ord u !tm;
      A.set low u !tm;
      Stack.push u st;
      G.Vertex.iter_adjacencies u (fun v ->
        if A.get ord v = 0
        then (visit v; A.set low u (min (A.get low u) (A.get low v)))
        else if A.get cmp v = 0
        then A.set low u (min (A.get low u) (A.get ord v)));
      if A.get ord u = A.get low u then
        let component = ref L.nil in
        let rec loop () =
          let v = Stack.pop st in
          A.set cmp v 1;
          component := L.cons v !component;
          if not (G.Vertex.eq u v) then loop () in
        (loop (); scc := LL.cons !component !scc) in
    G.Vertex.iter_universe (fun v ->
      if A.get ord v <= 0 then visit v);
    !scc
end
