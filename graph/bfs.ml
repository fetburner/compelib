module DirectedGraph
  (* 道の表現 *)
  (Path : sig
    type t
    (* 辺の名前 *)
    type edge
    (* 長さ0の道 *)
    val nil : t
    (* 道の後ろに辺を付け足した道 *)
    val snoc : t -> edge -> t
  end) :
sig
  (* BFSにより，重みのないグラフの最短経路を求める *)
  val bfs :
    (* 頂点数（Hashtblを用いるので目安程度） *)
    int ->
    (* 辺の名前が付いた隣接リスト *)
    ('v -> ('v * Path.edge) list) ->
    (* 始点 *)
    'v ->
    (* 最短経路を返す関数 辿り着けなければNoneを返す） *)
    ('v -> Path.t option)
end = struct
  let rec bfs_aux es d frontier t =
    try Some (Hashtbl.find d t) with
    | Not_found ->
        match !frontier with
        | [] -> None
        | _ :: _ ->
            frontier := List.fold_right (fun u ->
              List.fold_right (fun (v, e) frontier ->
                if Hashtbl.mem d v
                then frontier
                else (Hashtbl.add d v (Path.snoc (Hashtbl.find d u) e); v :: frontier))
              (es u)) !frontier [];
            bfs_aux es d frontier t

  (*
   * 終点に辿り着いた時点で探索を切り上げるが，
   * 戻り値の関数を覚えておくと，途中まで探索した結果が再利用される
   * （#trace bfs_aux すると分かりやすい）
   *)
  let bfs n es s =
    let d = Hashtbl.create n in
    Hashtbl.add d s Path.nil;
    let frontier = ref [s] in
    bfs_aux es d frontier
end

(* 経路情報も欲しい場合 *)
module G = DirectedGraph (struct
  type t = string list
  type edge = string
  let nil = []
  let snoc t e = e :: t
end)

let maze =
  [|"......";
    ".#####";
    "..#.#.";
    "..##..";
    "#....."|];;
G.bfs 30 (fun (i, j) ->
  [(i - 1, j); (i + 1, j); (i, j - 1); (i, j + 1)]
  |> List.filter (fun (i, j) -> try maze.(j).[i] = '.' with _ -> false)
  |> List.map (fun (i', j') -> ((i', j'), Printf.sprintf "(%d,%d)->(%d,%d)" i j i' j'))) (0, 0) (3, 2);;

let maze =
  [|"......";
    ".#####";
    "..#...";
    "..##..";
    "#....."|];;

let d = G.bfs 30 (fun (i, j) ->
  [(i - 1, j); (i + 1, j); (i, j - 1); (i, j + 1)]
  |> List.filter (fun (i, j) -> try maze.(j).[i] = '.' with _ -> false)
  |> List.map (fun (i', j') -> ((i', j'), Printf.sprintf "(%d,%d)->(%d,%d)" i j i' j'))) (0, 0);;
d (5, 0);;
(* 途中までの計算結果が再利用される *)
d (3, 2);;
    
(* 距離だけ欲しい場合 *)
module G = DirectedGraph (struct
  type t = int
  type edge = unit
  let nil = 0
  let snoc t _ = t + 1
end)

let d = G.bfs 30 (fun (i, j) ->
  [(i - 1, j); (i + 1, j); (i, j - 1); (i, j + 1)]
  |> List.filter (fun (i, j) -> try maze.(j).[i] = '.' with _ -> false)
  |> List.map (fun (i', j') -> ((i', j'), ()))) (0, 0);;
d (5, 0);;
d (3, 2);;
