module F
  (Vertices : sig
    type t (* グラフに含まれる頂点の集合 *)
    type vertex
    (* グラフに含まれる頂点についてのイテレータ *)
    val iter : (vertex -> unit) -> t -> unit
  end)
  (Weight : sig
    type t
    val min : t -> t -> t
    val ( + ) : t -> t -> t
    val is_finite : t -> bool (* 経路長が有限かどうかの判定 *)
  end)
  (* n * n の2次配列の実装 *)
  (SquareMatrix : sig
    type t
    type elt = Weight.t
    type key = Vertices.vertex
    val get : t -> key -> key -> elt
    val set : t -> key -> key -> elt -> unit
  end)
: sig
  type vertex = Vertices.vertex
  type weight = Weight.t
  type vertices = Vertices.t
  type adjacency_matrix = SquareMatrix.t

  val shortest_path :
    (* 頂点の数n *)
    vertices ->
    (* 隣接行列
       辺が存在しなければ無限大を入れること *)
    adjacency_matrix ->
    (* 最短距離で隣接行列を上書きする *)
    unit
end
