module type DirectedGraph = sig
  module Vertex : sig
    type t
    val get_distance : t -> int
    val set_distance : t -> int -> unit
    val iter_adjacencies : t -> (t -> unit) -> unit
  end
end

let shortest_path (type vertex)
  (module G : DirectedGraph
    with type Vertex.t = vertex) s =
  (* 始点への経路長を0にする *)
  G.Vertex.set_distance s 0;
  let w = ref 1 in
  let q = ref [s] in
  let rec bfs t =
    match !q with
    (* もう既に全ての頂点までの経路が分かっているので返す *)
    | [] -> G.Vertex.get_distance t
    | us ->
        match G.Vertex.get_distance t with
        (* 既に終点までの距離が分かっているので返す *)
        | x when x < !w -> x
        (* 終点までの距離が分かっていないので，BFSを続行 *)
        | _ ->
            q := [];
            List.iter (fun u ->
              G.Vertex.iter_adjacencies u @@ fun v ->
                if !w < G.Vertex.get_distance v then
                  (q := v :: !q; G.Vertex.set_distance v !w)) us;
            incr w;
            bfs t in
  bfs
