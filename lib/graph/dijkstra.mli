module type WeightedDirectedGraph = sig
  module Distance : sig
    type t
    val zero : t
    val compare : t -> t -> int
  end
  module Vertex : sig
    type t
    (* 現在の頂点への距離を取得 *)
    val get_distance : t -> Distance.t
    (* 現在の頂点への距離を代入 *)
    val set_distance : t -> Distance.t -> unit
    (* 最短経路を求めたいグラフの，ある頂点から伸びる辺に対してのイテレータ *)
    val iter_adjacencies : t -> (t -> Distance.t (* 辺を通った際の距離 *) -> unit) -> unit
  end
end

module F
  (* 経路長を優先度としたヒープの実装 *)
  (Heap : sig
    type t
    type elt (* 頂点に相当 *)
    type key (* 経路長に相当 *)

    (* 空なヒープを作成する *)
    val create : unit -> t
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
  type vertex = Heap.elt
  type distance = Heap.key

  (* ダイクストラ法で最短経路を求める関数 *)
  val shortest_path :
    (module WeightedDirectedGraph
      with type Vertex.t = vertex
       and type Distance.t = distance) ->
    (* 始点 *)
    vertex ->
    (* 終点を受け取って，始点からの最短距離を返す関数
       始点から辿り着けない場合，無限大を返す
       この関数を覚えておけば，呼び出しごとの途中までの計算結果がシェアされる *)
    (vertex -> distance)
end
