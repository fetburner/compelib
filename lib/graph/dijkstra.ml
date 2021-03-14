module type WeightedDirectedGraph = sig
  module Distance : sig
    type t
    val zero : t
    val compare : t -> t -> int
  end
  module Vertex : sig
    type t
    val get_distance : t -> Distance.t
    val set_distance : t -> Distance.t -> unit
    val iter_adjacencies : t -> (t -> Distance.t -> unit) -> unit
  end
end

module F
  (Heap : sig
    type t
    type elt
    type key
    val create : unit -> t
    val add : t -> key -> elt -> unit
    val take_min_binding : t -> (key * elt) option
  end)
= struct
  type vertex = Heap.elt
  type distance = Heap.key

  let shortest_path
    (module G : WeightedDirectedGraph with type Vertex.t = vertex and type Distance.t = distance) s =
    let q = Heap.create () in
    (* 始点への経路長を0にする *)
    G.Vertex.set_distance s G.Distance.zero;
    (* 既に最短距離が確定した辺へのクエリを高速化するため，
       ヒープの最小要素をメモしておく *)
    let min_binding = ref @@ Some (G.Distance.zero, s) in
    let rec dijkstra t =
      match !min_binding with
      (* もう既に全ての頂点までの距離が分かっている *)
      | None -> G.Vertex.get_distance t
      | Some (w, u) ->
          match G.Vertex.get_distance t with
          (* 既に終点までの距離が分かっているので返す *)
          | x when 0 <= G.Distance.compare w x -> x
          (* 終点までの距離が分かっていないので，ダイクストラ法を続行 *)
          | _ ->
              (* 未だ頂点uを訪れていない *)
              if 0 <= G.Distance.compare (G.Vertex.get_distance u) w then
                G.Vertex.iter_adjacencies u (fun v c ->
                  if 0 < G.Distance.compare (G.Vertex.get_distance v) c then
                    (Heap.add q c v; G.Vertex.set_distance v c));
              min_binding := Heap.take_min_binding q;
              dijkstra t in
    dijkstra
end
