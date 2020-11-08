module F
  (Weight : sig
    type t
    val compare : t -> t -> int
  end)
  (Edge : sig
    type t
    type v
    type w = Weight.t
    val weight : t -> w
    val vertex1 : t -> v
    val vertex2 : t -> v
  end)
  (Array : sig
    type 'a t
    type key = Edge.v
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

  let kruskal n es f acc =
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
