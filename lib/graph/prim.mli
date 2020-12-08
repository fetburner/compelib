(* 重み付き無向グラフ *)
module type WeightedUndirectedGraph = sig
  (* 辺の重み *)
  module Weight : sig
    type t
    val zero : t
    val compare : t -> t -> int
  end
  (* 頂点 *)
  module rec Vertex : sig
    type t
    type set
    (* 頂点の全体集合 *)
    val universe : set
    (* ある頂点を端点の一つとした辺についてのイテレータ *)
    val iter_connected_edges : t -> (Edge.t -> unit) -> unit
  end
  and Edge : sig
    type t
    (* 辺の重み *)
    val weight : t -> Weight.t
    (* 辺の端点の一つ *)
    val endpoint : t -> Vertex.t
  end
end

(* 重み付きの根なし木 *)
module type UnrootedWeightedTree = sig
  (* 辺の重み *)
  module Weight : sig
    type t
    val zero : t
    val compare : t -> t -> int
  end
  (* 頂点 *)
  module Vertex : sig
    type t
    type set
    (* 頂点の全体集合 *)
    val universe : set
  end
  (* 辺 *)
  module Edge : sig
    type t
    (* 辺の重み *)
    val weight : t -> Weight.t
    (* 辺の端点の一つ *)
    val endpoint : t -> Vertex.t
    (* グラフに含まれる全ての辺を畳み込む *)
    val fold_universe : (t -> 'a -> 'a) -> 'a -> 'a
  end
end

module F
  (* 頂点を添字，辺の重みを要素とした配列の実装 *)
  (Array : sig
    type t
    type key
    type elt
    type size
    (* 全ての頂点について無限大で初期化された配列を作る *)
    val make : size -> t
    val get : t -> key -> elt
    val set : t -> key -> elt -> unit
  end)
  (* 辺の重みを優先度としたヒープの実装 *)
  (Heap : sig
    type t
    type size = Array.size
    type elt
    type key = Array.elt (* 辺の重みに相当 *)
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
: sig
  type edge = Heap.elt
  type vertex = Array.key
  type weight = Array.elt
  type vertices = Array.size

  (* プリム法で最小全域木を求める *)
  val minimum_spanning_tree :
    (* 最小全域木を求めるグラフ
       ここで，Vertex.iter_connected_edges v f で，
       f に渡される辺 e に対して Edge.endpoint を呼び出すと，
       v でない方の端点が返される必要がある *)
    (module WeightedUndirectedGraph
      with type Edge.t = edge
       and type Weight.t = weight
       and type Vertex.t = vertex
       and type Vertex.set = vertices) ->
    (* 頂点の一つ（これは必ず最小全域木に含まれる） *)
    vertex ->
    (* 最小全域木 *)
    (module UnrootedWeightedTree
      with type Edge.t = edge
       and type Weight.t = weight
       and type Vertex.t = vertex
       and type Vertex.set = vertices)
end
