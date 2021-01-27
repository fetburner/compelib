(* 無向グラフ *)
module type UnweightedDirectedGraph = sig
  module Vertex : sig
    type t
    type set
    (* グラフに含まれる頂点の集合 *)
    val universe : set
    (* 頂点に含まれる集合の畳み込み *)
    val fold_universe : (t -> 'a -> 'a) -> 'a -> 'a
    (* 隣接する頂点の畳み込み *)
    val fold_adjacencies : t -> (t -> 'a -> 'a) -> 'a -> 'a
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
  (* 頂点を添字，真偽値を要素とした配列の実装
     A.make は false で初期化された配列を返さなくてはならない *)
  (A : Array with type elt = bool)
  (* 頂点のリスト *)
  (L : List with type elt = A.key)
: sig
  type vertex = A.key
  type vertices = A.size

  (* トポロジカルソート *)
  val sort :
    (* 有向グラフ *)
    (module UnweightedDirectedGraph
      with type Vertex.t = vertex
       and type Vertex.set = vertices) ->
    (* 頂点をトポロジカルソートしたリスト *)
    L.t
end

