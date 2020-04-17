module Weighted01DirectedGraph
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
  val bfs01 :
    (* 頂点数 *)
    int ->
    (* 辺の名前が付いた隣接リスト *)
    (* 辺の重さが0ならfalse，1ならtrue *)
    ('v -> ('v * Path.edge * bool) list) ->
    (* 始点 *)
    'v ->
    (* 最短経路を返す関数 辿り着けなければNoneを返す） *)
    ('v -> Path.t option)
end =
struct
  let bfs01 n es s =
    (* 始点sからの経路 *)
    let d = Hashtbl.create n in
    (* 現在BFSで走査している頂点のリスト *)
    let vps = ref [(s, Path.nil)] in
    let rec bfs t =
      match Hashtbl.find_opt d t, !vps with
      (* もう既に全ての頂点までの経路が分かっている *)
      | None, [] -> None
      (* 既に終点までの経路が分かっているので返す *)
      | Some _ as ans, _ -> ans
      (* 終点までの経路が分かっていないので，01BFSを続行 *)
      | None, _ :: _ -> vps := List.fold_left dfs [] !vps; bfs t
    (* 重さ0の辺をDFSで縮約 *)
    and dfs vps (v, p) =
      if Hashtbl.mem d v then vps
      else begin
        Hashtbl.add d v p;
        List.fold_left (fun vps (u, e, b) ->
          if b
          then (u, Path.snoc p e) :: vps
          else dfs vps (u, Path.snoc p e)) vps (es v)
      end
    in bfs
end

