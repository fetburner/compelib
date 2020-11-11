module F
  (Weight : sig
    type t
    val compare : t -> t -> int
  end)
  (Edge : sig
    type t
    type vertex
    type weight = Weight.t
    val weight : t -> weight
    val vertex1 : t -> vertex
    val vertex2 : t -> vertex
  end)
  (Array : sig
    type 'a t
    type key = Edge.vertex
    type size
    val get : 'a t -> key -> 'a
    val init : size -> (key -> 'a) -> 'a t
  end)
= struct
  type edge = Edge.t
  type size = Array.size
  type weight = Weight.t
  type vertex = Array.key

  module UF = UnionFind.Make (struct
    type t = unit
    let union _ _ = ()
  end)

  let minimum_spanning_tree n es f acc =
    let uf = Array.init n @@ fun _ -> UF.make () in
    List.fold_left (fun acc e ->
      let uf_u = Array.get uf (Edge.vertex1 e) in
      let uf_v = Array.get uf (Edge.vertex2 e) in
      if UF.equal uf_u uf_v
      then acc
      else (UF.unite uf_u uf_v; f e acc)) acc @@
    List.sort (fun e e' ->
      Weight.compare (Edge.weight e) (Edge.weight e')) es
end
