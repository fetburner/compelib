module F
  (* 辺の重み *)
  (Weight : sig
    type t
    val zero : t
    val compare : t -> t -> int
  end)
  (Edge : sig
    type t
    type v
    type w = Weight.t
    val weight : t -> w
    val vertex : t -> v
  end)
  (* 辺の重みを優先度としたヒープの実装 *)
  (Heap : sig
    type t
    type size
    type key = Edge.w (* 辺の重みに相当 *)
    type elt = Edge.t
    (* 空なヒープを作成する *)
    val make : size -> t
    (* ヒープが空ならNoneを，
       そうでなければ重みが最小となるbindingを一つ返す
       返したbindingはヒープから削除される *)
    val take_min_binding : t -> (key * elt) option
    (* ヒープにbindingを追加する
       既に同じ頂点についてのbindingが追加されていたら，
       重みが小さい方だけを残しても良いし，何も考えずに追加してもよい．
       前者の実装ならプリム法の実装が時間計算量O((V + E) log V)，
       空間計算量O(V)に改善する *)
    val add : t -> key -> elt -> unit
  end)
  (* 頂点を添字，辺の重みを要素とした配列の実装 *)
  (Array : sig
    type t
    type key = Edge.v
    type elt = Edge.w
    type size = Heap.size
    (* 全ての頂点について無限大で初期化された配列を作る *)
    val make : size -> t
    val get : t -> key -> elt
    val set : t -> key -> elt -> unit
  end)
: sig
  type edge = Edge.t
  type vertex = Edge.v
  type weight = Weight.t
  type vertices = Array.size

  (* プリム法で最小全域木を求める *)
  val minimum_spanning_tree :
    (* グラフに含まれる頂点の集合 *)
    vertices ->
    (* 最小全域木を求めたいグラフの，ある頂点から伸びる辺に対してのイテレータ *)
    (vertex -> (edge -> unit) -> unit) ->
    (* 頂点の一つ（これは必ず最小全域木に含まれる） *)
    vertex ->
    (* 最小全域木に含まれる辺のリストについての畳み込み *)
    (edge -> 'a -> 'a) -> 'a -> 'a
end
