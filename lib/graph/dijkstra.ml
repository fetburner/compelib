module type WeightedDirectedGraph = sig
  module Distance : sig
    type t
    val zero : t
    val compare : t -> t -> int
  end
  module Vertex : sig
    type t
    type set
    val universe : set
    val iter_adjacency : t -> (t -> (Distance.t -> Distance.t) -> unit) -> unit
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
    type elt = Array.key
    type key = Array.elt
    type size = Array.size
    val make : size -> t
    val add : t -> key -> elt -> unit
    val take_min_binding : t -> (key * elt) option
  end)
= struct
  type vertex = Array.key
  type distance = Array.elt
  type vertices = Array.size

  let shortest_path
    (module G : WeightedDirectedGraph
      with type Vertex.t = vertex
       and type Vertex.set = vertices
       and type Distance.t = distance) s =
    let q = Heap.make G.Vertex.universe in
    let d = Array.make G.Vertex.universe in
    (* 始点への経路長を0にする *)
    Array.set d s G.Distance.zero;
    (* 既に最短距離が確定した辺へのクエリを高速化するため，
       ヒープの最小要素をメモしておく *)
    let min_binding = ref @@ Some (G.Distance.zero, s) in
    let rec dijkstra t =
      match !min_binding with
      (* もう既に全ての頂点までの距離が分かっている *)
      | None -> Array.get d t
      | Some (w, u) ->
          match Array.get d t with
          (* 既に終点までの距離が分かっているので返す *)
          | x when 0 <= G.Distance.compare w x -> x
          (* 終点までの距離が分かっていないので，ダイクストラ法を続行 *)
          | _ ->
              (* 未だ頂点uを訪れていない *)
              if 0 <= G.Distance.compare (Array.get d u) w then
                G.Vertex.iter_adjacency u (fun v f ->
                  (* uからvに伸びる辺を通った際の経路長 *)
                  let c = f w in
                  if 0 < G.Distance.compare (Array.get d v) c then
                    (Heap.add q c v; Array.set d v c));
              min_binding := Heap.take_min_binding q;
              dijkstra t in
    dijkstra
end
