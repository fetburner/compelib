module type WeightedDirectedGraph = sig
  module Distance : sig
    type t
    val inf : t
    val zero : t
    val neg_inf : t
    val compare : t -> t -> int
  end
  module Vertex : sig
    type t
    val cardinal : int
    val get_distance : t -> Distance.t
    val set_distance : t -> Distance.t -> unit
    val compare : t -> t -> int
  end
  module Edge : sig
    type t
    val universe : t list
    val source : t -> Vertex.t
    val destination : t -> Vertex.t
    val add_weight : t -> Distance.t -> Distance.t
  end
end

let shortest_path (type vertex) (type distance)
  (module G : WeightedDirectedGraph
    with type Vertex.t = vertex
     and type Distance.t = distance) s =
  let es = List.sort (fun e e' ->
    let u = G.Edge.source e in
    let v = G.Edge.destination e in
    let u' = G.Edge.source e' in
    let v' = G.Edge.destination e' in
    match G.Vertex.compare u v, G.Vertex.compare u' v' with
    (* 自己辺同士は始点の小さい順に緩和 *)
    | 0, 0 -> G.Vertex.compare u u'
    (* 添字の小さい頂点から大きい頂点への辺同士は，始点の小さい順に緩和 *)
    | x, y when x < 0 && y < 0 -> G.Vertex.compare u u'
    (* 添字の大きい頂点から小さい頂点への辺同士は，始点の大きい順に緩和 *)
    | x, y when x > 0 && y > 0 -> G.Vertex.compare u' u
    (* 自己辺は添字の小さい頂点から大きい頂点への辺と一緒のタイミングで緩和する *)
    | 0, y when y < 0 ->
        begin match G.Vertex.compare u u' with
        | 0 -> -1
        | x -> x
        end
    | x, 0 when x < 0 ->
        begin match G.Vertex.compare u u' with
        | 0 -> 1
        | x -> x
        end
    (* 添字の小さい頂点から大きい頂点への辺の緩和が終わってから，
       添字の大きい頂点から小さい頂点への辺を緩和する *)
    | x, y when x < 0 && y > 0 -> -1
    | x, y when x > 0 && y < 0 -> 1
    | 0, y when y > 0 -> -1
    | x, 0 when x > 0 -> 1
    | _, _ -> failwith "bellman_ford") G.Edge.universe in
  G.Vertex.set_distance s G.Distance.zero;
  (* 残りの反復回数 *)
  let i = ref G.Vertex.cardinal in
  (* 負閉路とみなす閾値 *)
  let th = succ G.Vertex.cardinal lsr 1 in
  (* 更新が行われたか *)
  let is_modified = ref true in
  while 0 < !i && !is_modified do
    is_modified := false;
    List.iter (fun e ->
      (* 原点から到達できない頂点は更新しない *)
      let du = G.Vertex.get_distance (G.Edge.source e) in
      if 0 < G.Distance.compare G.Distance.inf du then
        let v = G.Edge.destination e in
        let dv =
          if 0 <= G.Distance.compare G.Distance.neg_inf du
          then G.Distance.neg_inf
          else G.Edge.add_weight e du in
        if 0 < G.Distance.compare (G.Vertex.get_distance v) dv then
          (is_modified := true; G.Vertex.set_distance v @@ if !i <= th then G.Distance.neg_inf else dv)) es;
      decr i
  done;
  G.Vertex.get_distance
