module Weighted01DirectedGraph
  (* 頂点を添字，経路長を要素とした配列の実装 *)
  (Array : sig
    type t
    type key
    type elt = int (* 全ての重さが0か1なグラフなので経路長は非負整数 *)
    val get : t -> key -> elt
    val set : t -> key -> elt -> unit
  end)
: sig
  type vertex = Array.key

  val shortest_path :
    (* 全ての頂点についての経路長が無限大で初期化された配列 *)
    Array.t ->
    (* 最短経路を求めたいグラフの，ある頂点から伸びる辺に対してのイテレータ
       重みが0の辺に対してはf0を，1の辺に対してはf1を用いる *)
    (vertex -> f0:(vertex -> unit) -> f1:(vertex -> unit) -> unit) ->
    (* 始点 *)
    vertex ->
    (* 終点を受け取って，始点からの最短距離を返す関数
       始点から辿り着けない場合，無限大を返す
       この関数を覚えておけば，呼び出しごとの途中までの計算結果がシェアされる *)
    (vertex -> int)
end =
struct
  type vertex = Array.key

  let shortest_path d es s =
    (* 始点への経路長を0にする *)
    Array.set d s 0;
    let w = ref 1 in
    let q = ref [s] in
    let rec bfs t =
      match !q with
      (* もう既に全ての頂点までの経路が分かっているので返す *)
      | [] -> Array.get d t
      | us ->
          match Array.get d t with
          (* 既に終点までの距離が分かっているので返す *)
          | x when x < !w -> x
          (* 終点までの距離が分かっていないので，BFSを続行 *)
          | _ ->
              q := [];
              List.iter dfs us;
              incr w;
              bfs t
    and dfs u =
      es u
        ~f0:(fun v ->
          if !w <= Array.get d v then
            (Array.set d v (!w - 1); dfs v))
        ~f1:(fun v ->
          if !w < Array.get d v then
            (Array.set d v !w; q := v :: !q)) in
    bfs
end

