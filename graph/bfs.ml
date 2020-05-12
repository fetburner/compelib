module DirectedGraph
  (* 頂点をキーとしたハッシュテーブルの実装 *)
  (VHash : Hashtbl.S)
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
  type 'a church_list = { fold : 'b. ('a -> 'b -> 'b) -> 'b -> 'b }

  (* BFSにより，重みのないグラフの最短経路を求める *)
  val bfs :
    (* 頂点数（ハッシュテーブルを用いるので目安程度） *)
    int ->
    (* 辺の名前が付いた隣接リスト *)
    (VHash.key -> (VHash.key * Path.edge) church_list) ->
    (* 始点 *)
    VHash.key ->
    (* 最短経路を返す関数 辿り着けなければNoneを返す） *)
    (VHash.key -> Path.t option)
end = struct
  type 'a church_list = { fold : 'b. ('a -> 'b -> 'b) -> 'b -> 'b }

  let rec bfs_aux es d frontier t =
    match VHash.find_opt d t, !frontier with
    (* もう既に全ての頂点までの経路が分かっている *)
    | None, [] -> None
    (* 既に終点までの経路が分かっているので返す *)
    | Some _ as ans, _ -> ans
    (* 終点までの経路が分かっていないので，BFSを続行 *)
    | None, _ :: _ ->
        frontier := List.fold_right (fun u ->
          (es u).fold @@ fun (v, e) frontier ->
            if VHash.mem d v
            then frontier
            else (VHash.add d v (Path.snoc (VHash.find d u) e); v :: frontier)) !frontier [];
        bfs_aux es d frontier t

  (*
   * 終点に辿り着いた時点で探索を切り上げるが，
   * 戻り値の関数を覚えておくと，途中まで探索した結果が再利用される
   * （#trace bfs_aux すると分かりやすい）
   *)
  let bfs n es s =
    let d = VHash.create n in
    VHash.add d s Path.nil;
    let frontier = ref [s] in
    bfs_aux es d frontier
end

module IntPairHash = Hashtbl.Make
  (struct
    type t = int * int
    let equal = ( = )
    let hash = Hashtbl.hash
  end)

(* 経路情報も欲しい場合 *)
module G = DirectedGraph
(IntPairHash)
(struct
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

let d = G.bfs 30 (fun (i, j) ->
  { G.fold = fun f ->
    List.fold_right (fun (i', j') acc ->
      match maze.(j').[i'] = '.' with
      | false | exception (Invalid_argument _) -> acc
      | true -> f ((i', j'), Printf.sprintf "(%d,%d)->(%d,%d)" i j i' j') acc)
    [(i - 1, j); (i + 1, j); (i, j - 1); (i, j + 1)] }) (0, 0);;
d (5, 0);;
(* 途中までの計算結果が再利用される *)
d (3, 2);;

(* 距離だけ欲しい場合 *)
module G = DirectedGraph (IntPairHash)
(struct
  type t = int
  type edge = unit
  let nil = 0
  let snoc t _ = t + 1
end)

let d = G.bfs 30 (fun (i, j) ->
  { G.fold = fun f ->
    List.fold_right (fun (i, j) acc ->
      match maze.(j).[i] = '.' with
      | false | exception (Invalid_argument _) -> acc
      | true -> f ((i, j), ()) acc)
    [(i - 1, j); (i + 1, j); (i, j - 1); (i, j + 1)] }) (0, 0);;
d (5, 0);;
d (3, 2);;
