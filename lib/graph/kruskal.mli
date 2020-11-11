module F
  (Weight : sig
    type t
    val compare : t -> t -> int
  end)
  (Edge : sig
    type t
    type vertex
    type weight = Weight.t
    val weight : t -> weight
    val vertex1 : t -> vertex
    val vertex2 : t -> vertex
  end)
  (Array : sig
    type 'a t
    type key = Edge.vertex
    type size
    val get : 'a t -> key -> 'a
    val init : size -> (key -> 'a) -> 'a t
  end)
: sig
  type edge = Edge.t
  type size = Array.size
  type weight = Weight.t
  type vertex = Array.key

  val minimum_spanning_tree :
    (* 頂点の数n *)
    size ->
    (* 辺のリスト
       ソートにList.sortを使っているせいでデータ構造が固定されている *)
    edge list ->
    (* 最小全域木に含まれる辺のリストについての畳み込み
       畳み込む関数と初期値まで与えられて初めて計算を開始するので，
       最小全域木に対して色々な操作をしたい場合は，
       一度リストにして覚えておくとよい．
       最小全域木の重さの和だけ欲しいとか一回しか使わない場合は，
       直接畳み込む関数と初期値を与えるとdeforestationになっていい感じ． *)
    (edge -> 'a -> 'a) -> 'a -> 'a
end
