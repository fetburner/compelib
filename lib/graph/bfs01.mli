module F
  (* 頂点を添字，経路長を要素とした配列の実装 *)
  (Array : sig
    type t
    type key
    type elt = int (* 全ての重さが0か1なグラフなので経路長は非負整数 *)
    type size (* 頂点数 *)
    val make : size -> t (* 全ての頂点についての経路長が無限大で初期化された配列を作る *)
    val get : t -> key -> elt
    val set : t -> key -> elt -> unit
  end)
: sig
  type vertex = Array.key
  type vertices = Array.size

  val shortest_path :
    (* 頂点数 *)
    vertices ->
    (* 最短経路を求めたいグラフの，ある頂点から伸びる辺に対してのイテレータ
       重みが0の辺に対してはf0を，1の辺に対してはf1を用いる *)
    (vertex -> f0:(vertex -> unit) -> f1:(vertex -> unit) -> unit) ->
    (* 始点 *)
    vertex ->
    (* 終点を受け取って，始点からの最短距離を返す関数
       始点から辿り着けない場合，無限大を返す
       この関数を覚えておけば，呼び出しごとの途中までの計算結果がシェアされる *)
    (vertex -> int)
end
