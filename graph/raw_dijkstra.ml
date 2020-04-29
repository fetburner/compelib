module WeightedDirectedGraph
  (* 辺の重み *)
  (Weight : sig
    type t
    val inf : t (* max_intとかを突っ込むとオーバーフローで死ぬので気をつけること *)
    val zero : t
    val ( + ) : t -> t -> t
    val compare : t -> t -> int
  end) :
sig
  (* 頂点を[0, n)の自然数に限定したグラフに対してのダイクストラ法 *)
  val raw_dijkstra :
    (* 頂点数 *)
    int ->
    (* 隣接リスト *)
    (int -> (int * Weight.t) list) ->
    (* 始点 *)
    int ->
    (* 始点から辿り着けなければWeight.infを返す関数 *)
    (int -> Weight.t)
end =
struct
  module WMap = Map.Make (Weight)

  let raw_dijkstra n es s =
    let d = Array.make n Weight.inf in
    d.(s) <- Weight.zero;
    let rec dijkstra_aux q =
      match WMap.min_binding_opt q with
      | None -> ()
      | Some (w, us) ->
          dijkstra_aux @@
          ListLabels.fold_left ~init:(WMap.remove w q) us ~f:(fun q u ->
            if 0 < Weight.compare w d.(u)
            then q
            else ListLabels.fold_left ~init:q (es u) ~f:(fun q (v, c) ->
              let open Weight in
              if Weight.compare d.(v) (w + c) <= 0
              then q
              else begin
                d.(v) <- w + c;
                WMap.add (w + c) (v :: try WMap.find (w + c) q with Not_found -> []) q
              end)) in
    dijkstra_aux (WMap.singleton Weight.zero [s]);
    Array.get d
end
