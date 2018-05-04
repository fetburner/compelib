module WeightedDirectedGraph
  (Weight : sig
    type t
    val zero : t
    val ( + ) : t -> t -> t
    val compare : t -> t -> int
  end) :
sig
  val warshall_floyd :
    (* 頂点のリスト *)
    'v list ->
    (* 辺のリスト *)
    ('v * 'v * Weight.t) list ->
    (* 辿り着けなければNoneを返す *)
    ('v -> 'v -> Weight.t option)
end =
struct
  let warshall_floyd vs es =
    (* 準備 *)
    (* dに入ってない2頂点の距離は無限とみなす *)
    let d =
      let n = List.length vs in
      Hashtbl.create (n * n) in
    List.iter (fun (u, v, c) ->
      Hashtbl.replace d (u, v) c) es;
    List.iter (fun v ->
      Hashtbl.replace d (v, v) Weight.zero) vs;
    (* メインループ *)
    List.iter (fun i ->
      List.iter (fun j ->
        List.iter (fun k ->
          (* d.(j).(k) <- min d.(j).(k) (d.(j).(i) + d.(i).(k)) *)
          let open Weight in
          match Hashtbl.find d (j, i) + Hashtbl.find d (i, k) with
          | exception Not_found -> () (* d.(j).(i) + d.(i).(k) は無限大 *)
          | djk' ->
              (* djk' = d.(j).(i) + d.(i).(k) *)
              if
                (* d.(j).(k) > d.(j).(i) + d.(i).(k) *)
                try 0 < compare (Hashtbl.find d (j, k)) djk'
                with Not_found -> true (* d.(j).(k) は無限大 *)
              then Hashtbl.replace d (j, k) djk') vs) vs) vs;
    fun u v -> try Some (Hashtbl.find d (u, v)) with Not_found -> None
end

(* sample code *)

module G = WeightedDirectedGraph (struct
  type t = int
  let zero = 0
  let ( + ) = ( + )
  let compare = compare
end)

let d = G.warshall_floyd [0; 1; 2; 3; 4; 5]
  [ (0, 1, 4); (0, 4, 3);
    (1, 0, 4); (1, 2, 2);
    (2, 1, 2); (2, 3, 3); (2, 4, 2);
    (3, 2, 3); (3, 4, 7);
    (4, 0, 3); (4, 2, 2); (4, 3, 7) ];;
Array.init 6 (fun i -> Array.init 6 (d i));;
