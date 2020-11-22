module F
  (* 頂点を添字とした整数の配列の実装 *)
  (Array : sig
    type t
    type key
    type size (* 頂点数 *)
    type elt = int
    val make : size -> t (* 中身は何でもよい *)
    val get : t -> key -> elt
    val set : t -> key -> elt -> unit
  end)
: sig
  type vertex = Array.key
  type vertices = Array.size

  (* 最小共通祖先を求める *)
  val lowest_common_ancestor :
    (* 根付き木の頂点数 *)
    vertices ->
    (* 根付き木の，ある頂点から延びる辺に対してのイテレータ *)
    (vertex -> (vertex -> unit) -> unit) ->
    (* 始点 *)
    vertex ->
    (* 各頂点の始点からの距離 *)
    (vertex -> int) ->
    (* 最小共通祖先を求める関数
       ここまでの引数が与えられた時点で前処理が行われるので，
       この関数を覚えておけば高速に動作する *)
    (vertex -> vertex -> vertex)
end
