module F
  (Vertex : sig
    type t
    type universe (* グラフに含まれる頂点の集合 *)
    (* グラフに含まれる頂点についてのイテレータ *)
    val universe_iter : (t -> unit) -> universe -> unit
  end)
  (Weight : sig
    type t
    val zero : t
    (* 頂点 v に長さ w の自己辺があったとき， v を通る最短経路はどれだけ長くなるか
       返り値は必ず0以下でなくてはならない *)
    val self : t -> t
    val min : t -> t -> t
    val ( + ) : t -> t -> t
    val is_infinite : t -> bool (* 経路長が無限大かどうかの判定 *)
  end)
  (* n * n の2次配列の実装 *)
  (SquareMatrix : sig
    type t
    type key = Vertex.t
    type elt = Weight.t
    type size = Vertex.universe
    (* 全要素が無限大で初期化された， n * n の2次配列を作る *)
    val make : size -> t
    val get : t -> key -> key -> elt
    val set : t -> key -> key -> elt -> unit
  end)
: sig
  type vertex = Vertex.t
  type weight = Weight.t
  type vertices = Vertex.universe

  val shortest_path :
    (* 頂点の数n *)
    vertices ->
    (* 辺についてのイテレータ *)
    ((vertex -> vertex -> weight -> unit) -> unit) ->
    (* 始点と終点を受け取って，辿り着けるならば最短距離を，
       辿り着けなければ無限大を返す関数 *)
    (vertex -> vertex -> weight)
end
