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
  (* 配列版の実装 *)
  val raw_warshall_floyd :
    (* 頂点の数n *)
    int ->
    (* 辺のリスト *)
    (* 頂点は0からn-1までの整数でなくてはならない *)
    (int * int * Weight.t) list ->
    (* 辿り着けなければNoneを返す *)
    (int -> int -> Weight.t option)

  (* 座標圧縮により，頂点に様々な型を使えるようにしたバージョン *)
  val warshall_floyd :
    (* 辺のリスト *)
    (Vertex.t * Vertex.t * Weight.t) list ->
    (* 辿り着けなければNoneを返す *)
    (Vertex.t -> Vertex.t -> Weight.t option)
end =
struct
  module CC = CoordComp (Vertex)

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

  let warshall_floyd es =
    let (n, comp, _) =
      CC.compress @@
        List.concat @@
          List.map (fun (u, v, _) -> [u; v]) es in
    let d =
      raw_warshall_floyd n @@
        List.map (fun (u, v, c) -> (comp u, comp v, c)) es in
    fun u v ->
      try d (comp u) (comp v) with Not_found -> None
end

(* sample code *)

module Int = struct
  type t = int
  let zero = 0
  let ( + ) = ( + )
  let compare = compare
end

module G = WeightedDirectedGraph (Int) (Int)

let d = G.raw_warshall_floyd 6
  [ (0, 1, 4); (0, 4, 3);
    (1, 0, 4); (1, 2, 2);
    (2, 1, 2); (2, 3, 3); (2, 4, 2);
    (3, 2, 3); (3, 4, 7);
    (4, 0, 3); (4, 2, 2); (4, 3, 7) ];;
Array.init 6 (fun i -> Array.init 6 (d i));;

let d = G.warshall_floyd
  [ (0, 1, 4); (0, 4, 3);
    (1, 0, 4); (1, 2, 2);
    (2, 1, 2); (2, 3, 3); (2, 4, 2);
    (3, 2, 3); (3, 4, 7);
    (4, 0, 3); (4, 2, 2); (4, 3, 7) ];;
Array.init 6 (fun i -> Array.init 6 (d i));;
