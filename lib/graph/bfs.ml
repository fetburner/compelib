module type DirectedGraph = sig
  module Vertex : sig
    type t
    type set
    val universe : set
    val iter_adjacency : t -> (t -> unit) -> unit
  end
end

module F
  (Array : sig
    type t
    type key
    type size
    type elt = int
    val make : size -> t
    val get : t -> key -> elt
    val set : t -> key -> elt -> unit
  end)
= struct
  type distance = int
  type vertex = Array.key
  type vertices = Array.size

  let shortest_path
    (module G : DirectedGraph
      with type Vertex.t = vertex
       and type Vertex.set = vertices) s =
    let d = Array.make G.Vertex.universe in
    (* 始点への経路長を0にする *)
    Array.set d s 0;
    let w = ref 1 in
    let q = ref [s] in
    let rec bfs t =
      match !q with
      (* もう既に全ての頂点までの経路が分かっているので返す *)
      | [] -> Array.get d t
      | us ->
          match Array.get d t with
          (* 既に終点までの距離が分かっているので返す *)
          | x when x < !w -> x
          (* 終点までの距離が分かっていないので，BFSを続行 *)
          | _ ->
              q := [];
              List.iter (fun u ->
                G.Vertex.iter_adjacency u @@ fun v ->
                  if !w < Array.get d v then
                    (q := v :: !q; Array.set d v !w)) us;
              incr w;
              bfs t in
    bfs
end
