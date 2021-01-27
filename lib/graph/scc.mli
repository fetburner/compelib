(* 無向グラフ *)
module type UnweightedDirectedGraph = sig
  module Vertex : sig
    type t
    type set
    (* グラフに含まれる頂点の集合 *)
    val universe : set
    (* 頂点の等価性判定 *)
    val eq : t -> t -> bool
    (* 頂点に含まれる集合の畳み込み *)
    val iter_universe : (t -> unit) -> unit
    (* 隣接する頂点の畳み込み *)
    val iter_adjacencies : t -> (t -> unit) -> unit
  end
end

(* リスト *)
module type List = sig
  type t
  type elt
  val nil : t
  val cons : elt -> t -> t
end

module type Array = sig
  type t
  type elt
  type key
  type size
  val make : size -> t
  val get : t -> key -> elt
  val set : t -> key -> elt -> unit
end

module F
  (* 頂点を添字，整数値を要素とした配列の実装
     A.make は 0 で初期化された配列を返さなくてはならない *)
  (A : Array with type elt = int)
  (* 頂点のリスト *)
  (L : List with type elt = A.key)
  (* 頂点のリストのリスト *)
  (LL : List with type elt = L.t)
: sig
  type vertex = A.key
  type vertices = A.size

  (* 強連結成分分解 *)
  val scc :
    (* 有向グラフ *)
    (module UnweightedDirectedGraph
      with type Vertex.t = vertex
       and type Vertex.set = vertices) ->
    (* 強連結成分のリスト *)
    LL.t
end
