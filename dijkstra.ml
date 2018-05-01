module WeightedDirectedGraph
  (Vertex : sig
    type t
    val compare : t -> t -> int
  end)
  (Weight : sig
    type t
    val inf : t
    val zero : t
    val ( + ) : t -> t -> t
    val compare : t -> t -> int
  end) :
sig
  val dijkstra :
    Vertex.t list ->
    (Vertex.t -> (Vertex.t * Weight.t) list) ->
    Vertex.t ->
    (Vertex.t -> Weight.t)
end =
struct
  module WMap = Map.Make (Weight)
  module VSet = Set.Make (Vertex)
  module VMap = Map.Make (Vertex)

  (* ヒープに要素を追加 *)
  let add k p pk =
    WMap.add p
      (VSet.add k
        (try WMap.find p pk with Not_found -> VSet.empty)) pk

  (* ヒープから要素を削除 *)
  let remove k p pk =
    let kset = WMap.find p pk in
    if VSet.cardinal kset <= 1
    then WMap.remove p pk
    else WMap.add p (VSet.remove k kset) pk

  (* ダイクストラ法のメインループ *)
  let rec dijkstra_aux e (q, d) =
    match WMap.min_binding q with
    | exception Not_found -> d
    | (w, vs) ->
        dijkstra_aux e @@ VSet.fold (fun v ->
          (* w = d.(v) *)
          List.fold_right (fun (u, c) (q, d) ->
            (* c は頂点 v から頂点 u への辺の重み *)
            let open Weight in
            (* w' = d.(u) *)
            let w' = VMap.find u d in
            (* d.(u) <= d.(v) + c *)
            if 0 <= compare (w + c) w'
            then (q, d)
            else (add u (w + c) (remove u w' q), VMap.add u (w + c) d)) (e v)) vs (WMap.remove w q, d)

  let rec dijkstra vs e s =
    let d =
      dijkstra_aux e
        (WMap.add Weight.zero (VSet.singleton s)
          (WMap.singleton Weight.inf
            (VSet.remove s
              (List.fold_right VSet.add vs VSet.empty))),
         VMap.add s Weight.zero
           (List.fold_right (fun v ->
             VMap.add v Weight.inf) vs VMap.empty)) in
    fun v -> VMap.find v d
end

(* sample code *)

module Int = struct
  type t = int
  let compare = compare
end

module Float = struct
  type t = float
  let zero = 0.
  let inf = infinity
  let ( + ) = ( +. )
  let compare = compare
end

module G = WeightedDirectedGraph (Int) (Float)

let d = G.dijkstra [0; 1; 2; 3; 4; 5] (function
  | 0 -> [ (1, 7.); (2, 9.); (5, 14.) ]
  | 1 -> [ (0, 7.); (2, 10.); (3, 15.) ]
  | 2 -> [ (0, 9.); (1, 10.); (3, 11.); (5, 2.) ]
  | 3 -> [ (1, 15.); (2, 11.); (4, 6.) ]
  | 4 -> [ (3, 6.); (5, 9.) ]
  | 5 -> [ (0, 14.); (2, 2.); (4, 9.) ]) 0;;
Array.init 6 d
