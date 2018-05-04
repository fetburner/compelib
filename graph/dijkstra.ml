module WeightedDirectedGraph
  (Vertex : sig
    type t
    val compare : t -> t -> int
  end)
  (Weight : sig
    type t
    val zero : t
    val ( + ) : t -> t -> t
    val compare : t -> t -> int
  end) :
sig
  val dijkstra :
    (* 隣接リスト *)
    (Vertex.t -> (Vertex.t * Weight.t) list) ->
    (* 始点 *)
    Vertex.t ->
    (* 始点から辿り着けなければNoneを返す *)
    (Vertex.t -> Weight.t option)
end =
struct
  module WMap = Map.Make (Weight)
  module VMap = Map.Make (Vertex)

  (* ダイクストラ法のメインループ *)
  (* d に入っていない頂点への距離は無限大とみなす *)
  let rec dijkstra_aux e (q, d) =
    match WMap.min_binding q with
    | exception Not_found -> d
    | (w, vs) ->
        dijkstra_aux e @@ List.fold_right (fun v ->
          (* w = d.(v) *)
          List.fold_right (fun (u, c) (q, d) ->
            (* c は頂点 v から頂点 u への辺の重み *)
            let open Weight in
            if
              (* d.(u) <= d.(v) + c *)
              try 0 <= compare (w + c) (VMap.find u d)
              with Not_found -> false (* d.(u) は無限大 *)
            then (q, d)
            else
              (WMap.add (w + c) (u :: try WMap.find (w + c) q with Not_found -> []) q,
               VMap.add u (w + c) d)) (e v)) vs (WMap.remove w q, d)

  let rec dijkstra e s =
    let d =
      dijkstra_aux e
        (WMap.singleton Weight.zero [s],
         VMap.singleton s Weight.zero) in
    fun v -> try Some (VMap.find v d) with Not_found -> None
end

(* sample code *)

module Int = struct
  type t = int
  let zero = 0
  let ( + ) = ( + )
  let compare = compare
end

module G = WeightedDirectedGraph (Int) (Int)

let d = G.dijkstra (function
  | 0 -> [ (1, 7); (2, 9); (5, 14) ]
  | 1 -> [ (0, 7); (2, 10); (3, 15) ]
  | 2 -> [ (0, 9); (1, 10); (3, 11); (5, 2) ]
  | 3 -> [ (1, 15); (2, 11); (4, 6) ]
  | 4 -> [ (3, 6); (5, 9) ]
  | 5 -> [ (0, 14); (2, 2); (4, 9) ]) 0;;
Array.init 7 d
