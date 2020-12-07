module type WeightedDirectedGraph = sig
  module Distance : sig
    type t
    val zero : t
    val compare : t -> t -> int
  end
  module Vertex : sig
    type t
    type set
    (* グラフに含まれる頂点の集合 *)
    val universe : set
    (* 最短経路を求めたいグラフの，ある頂点から伸びる辺に対してのイテレータ *)
    val iter_adjacency : t -> (t -> (Distance.t -> Distance.t) (* 辺を通った際の距離を加算する関数 *) -> unit) -> unit
  end
end

module F
  (* 頂点を添字，経路長を要素とした配列の実装 *)
  (Array : sig
    type t
    type key
    type elt
    type size

    (* 全ての頂点についての経路長が無限大で初期化された配列を作る *)
    val make : size -> t
    val get : t -> key -> elt
    val set : t -> key -> elt -> unit
  end)
  (* 経路長を優先度としたヒープの実装 *)
  (Heap : sig
    type t
    type elt = Array.key
    type key = Array.elt (* 経路長に相当 *)
    type size = Array.size

    (* 空なヒープを作成する *)
    val make : size -> t
    (* ヒープに binding を追加する
       既に同じ頂点についての binding が追加されていたら，
       経路長の短い方だけを残しても良いし，何も考えずに追加してもよい．
       前者の実装ならダイクストラ法の実装が時間計算量 O((V + E) log V)，
       空間計算量 O(V) に改善する *)
    val add : t -> key -> elt -> unit
    (* ヒープが空なら None を，
       そうでなければ経路長が最小となる binding を返す
       返した binding はヒープから削除される *)
    val take_min_binding : t -> (key * elt) option
  end)
: sig
  type vertex = Array.key
  type distance = Array.elt
  type vertices = Array.size

  (* ダイクストラ法で最短経路を求める関数 *)
  val shortest_path :
    (module WeightedDirectedGraph
      with type Vertex.t = vertex
       and type Vertex.set = vertices
       and type Distance.t = distance) ->
    (* 始点 *)
    vertex ->
    (* 終点を受け取って，始点からの最短距離を返す関数
       始点から辿り着けない場合，無限大を返す
       この関数を覚えておけば，呼び出しごとの途中までの計算結果がシェアされる *)
    (vertex -> distance)
end
