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
  let rec dijkstra_aux e (d, q) =
    match WMap.min_binding q with
    | exception Not_found -> d
    | (w, us) ->
        dijkstra_aux e @@ List.fold_left (fun (d, q) u ->
          if Weight.compare (VMap.find u d) w < 0
          (* 既に頂点uを訪れていた *)
          then (d, q)
          (* w = d.(u) *)
          else List.fold_left (fun (d, q) (v, c) ->
            let open Weight in
            if
              (* d.(v) <= d.(u) + c *)
              try Weight.compare (VMap.find v d) (w + c) <= 0
              with Not_found -> false (* d.(u) は無限大 *)
            then (d, q)
            else
              VMap.add v (w + c) d,
              WMap.add (w + c) (v :: try WMap.find (w + c) q with Not_found -> []) q)
          (d, q) (e u)) (d, WMap.remove w q) us

  let dijkstra e s =
    let d =
      dijkstra_aux e
        (VMap.singleton s Weight.zero, WMap.singleton Weight.zero [s]) in
    fun t -> try Some (VMap.find t d) with Not_found -> None
end

(* sample code *)

module Int = struct
  type t = int
  let compare = compare
end

module G = WeightedDirectedGraph (Int)
  (struct
    type t = float
    let zero = 0.
    let ( + ) = ( +. )
    let compare = compare
  end)

let e =
  [|[ (1, 7.); (2, 9.); (5, 14.) ];
    [ (0, 7.); (2, 10.); (3, 15.) ];
    [ (0, 9.); (1, 10.); (3, 11.); (5, 2.) ];
    [ (1, 15.); (2, 11.); (4, 6.) ];
    [ (3, 6.); (5, 9.) ];
    [ (0, 14.); (2, 2.); (4, 9.) ]|];;

Array.init 7 (G.dijkstra (fun u -> e.(u)) 0);;

module G' = WeightedDirectedGraph (Int)
  (struct
    type t = float * (string list -> string list)
    let zero = (0., fun xs -> xs)
    let ( + ) (c, f) (d, g) = (c +. d, fun xs -> f (g xs))
    let compare (c, _) (d, _) = compare c d
  end)

let e' =
  Array.mapi (fun u ->
    List.map (fun (v, c) ->
      let s = Printf.sprintf "%d->%d" u v in
      (v, (c, fun xs -> s :: xs)))) e;;

Array.map (fun (Some (c, f)) -> (c, f [])) @@ Array.init 6 (G'.dijkstra (fun u -> e'.(u)) 0);;
