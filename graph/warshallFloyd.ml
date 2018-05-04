module WeightedDirectedGraph
  (Weight : sig
    type t
    val zero : t
    val ( + ) : t -> t -> t
    val compare : t -> t -> int
  end) :
sig
  (* 配列版の実装 *)
  val raw_warshall_floyd :
    (* 頂点の数n *)
    int ->
    (* 辺のリスト *)
    (* 頂点は0からn-1まで整数でなくてはならない *)
    (int * int * Weight.t) list ->
    (* 辿り着けなければNoneを返す *)
    (int -> int -> Weight.t option)

  val warshall_floyd :
    (* 頂点のリスト *)
    'v list ->
    (* 辺のリスト *)
    ('v * 'v * Weight.t) list ->
    (* 辿り着けなければNoneを返す *)
    ('v -> 'v -> Weight.t option)
end =
struct
  let raw_warshall_floyd n es =
    (* 準備 *)
    (* Noneで無限を表す *)
    let d = Array.make_matrix n n None in
    List.iter (fun (u, v, c) -> d.(u).(v) <- Some c) es;
    for v = 0 to n - 1 do
      d.(v).(v) <- Some Weight.zero
    done;
    for i = 0 to n - 1 do
      for j = 0 to n - 1 do
        for k = 0 to n - 1 do
          (* d.(j).(k) <- min d.(j).(k) (d.(j).(i) + d.(i).(k)) *)
          let open Weight in
          match d.(j).(k), d.(j).(i), d.(i).(k) with
          | _, None, _ -> ()
          | _, _, None -> ()
          (* d.(j).(k) <= d.(j).(i) + d.(i).(k) *)
          | Some djk, Some dji, Some dik when compare djk (dji + dik) <= 0 -> ()
          | _, Some dji, Some dik -> d.(j).(k) <- Some (dji + dik)
        done
      done
    done;
    fun u v ->
      if u < 0 || n <= u || v < 0 || n <= v
      then None
      else d.(u).(v)

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

let d = G.raw_warshall_floyd 6
  [ (0, 1, 4); (0, 4, 3);
    (1, 0, 4); (1, 2, 2);
    (2, 1, 2); (2, 3, 3); (2, 4, 2);
    (3, 2, 3); (3, 4, 7);
    (4, 0, 3); (4, 2, 2); (4, 3, 7) ];;
Array.init 6 (fun i -> Array.init 6 (d i));;

let d = G.warshall_floyd [0; 1; 2; 3; 4; 5]
  [ (0, 1, 4); (0, 4, 3);
    (1, 0, 4); (1, 2, 2);
    (2, 1, 2); (2, 3, 3); (2, 4, 2);
    (3, 2, 3); (3, 4, 7);
    (4, 0, 3); (4, 2, 2); (4, 3, 7) ];;
Array.init 6 (fun i -> Array.init 6 (d i));;
