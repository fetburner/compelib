module type DirectedGraph = sig
  module Vertex : sig
    type t
    type set
    (* グラフに含まれる頂点の集合 *)
    val universe : set
    (* 最短経路を求めたいグラフの，ある頂点から伸びる辺に対してのイテレータ *)
    val iter_adjacencies : t -> (t -> unit) -> unit
  end
end

module F
  (* 頂点を添字，経路長を要素とした配列の実装 *)
  (Array : sig
    type t
    type key
    type elt = int (* 重みのない（= 全ての重さが1な）グラフなので経路長は非負整数 *)
    type size (* 頂点数 *)
    val make : size -> t (* 全ての頂点についての経路長が無限大で初期化された配列を作る *)
    val get : t -> key -> elt
    val set : t -> key -> elt -> unit
  end)
: sig
  type distance = int
  type vertex = Array.key
  type vertices = Array.size

  (* BFSにより，重みのないグラフの最短経路長を求める *)
  val shortest_path :
    (module DirectedGraph
      with type Vertex.t = vertex
       and type Vertex.set = vertices) ->
    (* 始点 *)
    vertex ->
    (* 終点を受け取って，始点からの最短距離を返す関数
       始点から辿り着けない場合，無限大を返す
       この関数を覚えておけば，呼び出しごとの途中までの計算結果がシェアされる *)
    (vertex -> distance)
end
