module F
  (* 辺の重み *)
  (Weight : sig
    type t
    val zero : t
    val compare : t -> t -> int
  end)
  (* 経路長を優先度としたヒープの実装 *)
  (Heap : sig
    type t
    type elt (* 頂点に相当 *)
    type key = Weight.t (* 経路長に相当 *)
    (* ヒープが空ならNoneを，
       そうでなければ経路長が最小となるbindingを返す
       返したbindingはヒープから削除される *)
    val take_min_binding : t -> (key * elt) option
    (* ヒープにbindingを追加する
       既に同じ頂点についてのbindingが追加されていたら，
       経路長の短い方だけを残しても良いし，何も考えずに追加してもよい．
       前者の実装ならダイクストラ法の実装が時間計算量O((V + E) log V)，
       空間計算量O(V)に改善する *)
    val add : t -> key -> elt -> unit
  end)
  (* 頂点を添字，経路長を要素とした配列の実装 *)
  (Array : sig
    type t
    type key = Heap.elt
    type elt = Heap.key
    val get : t -> key -> elt
    val set : t -> key -> elt -> unit
  end)
: sig
  type weight = Array.elt
  type vertex = Array.key

  (* ダイクストラ法で最短経路を求める関数 *)
  val shortest_path :
    (* 空なヒープ *)
    Heap.t ->
    (* 全ての頂点についての経路長が無限大で初期化された配列 *)
    Array.t ->
    (* 最短経路を求めたいグラフの，ある頂点から伸びる辺に対してのイテレータ *)
    (vertex -> (vertex -> (weight -> weight) (* 辺を通った際のコストを加算する関数 *) -> unit) -> unit) ->
    (* 始点 *)
    vertex ->
    (* 終点を受け取って，始点からの最短距離を返す関数
       始点から辿り着けない場合，無限大を返す
       この関数を覚えておけば，呼び出しごとの途中までの計算結果がシェアされる *)
    (vertex -> weight)
end
