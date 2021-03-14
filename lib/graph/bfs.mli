module type DirectedGraph = sig
  module Vertex : sig
    type t
    (* 現在の頂点までの経路長を取得する *)
    val get_distance : t -> int
    (* 頂点までの経路長を上書きする *)
    val set_distance : t -> int -> unit
    (* 最短経路を求めたいグラフの，ある頂点から伸びる辺に対してのイテレータ *)
    val iter_adjacencies : t -> (t -> unit) -> unit
  end
end

(* BFSにより，重みのないグラフの最短経路長を求める *)
val shortest_path :
  (* グラフに含まれる頂点への経路長は全て無限大で初期化されている必要がある *)
  (module DirectedGraph with type Vertex.t = 'vertex) ->
  (* 始点 *)
  'vertex ->
  (* 終点を受け取って，始点からの最短距離を返す関数
     始点から辿り着けない場合，無限大を返す
     この関数を覚えておけば，呼び出しごとの途中までの計算結果がシェアされる *)
  ('vertex -> int)
