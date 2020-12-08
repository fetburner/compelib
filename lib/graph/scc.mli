(* 重み付き無向グラフ *)
module type UnweightedDirectedGraph = sig
  module Vertex : sig
    type t
    type set
    (* グラフに含まれる頂点の集合 *)
    val universe : set
    (* 頂点に含まれる集合の畳み込み *)
    val universe_fold : (t -> 'a -> 'a) -> 'a -> 'a
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

module F
  (* 頂点を添字，真偽値を要素とした配列の実装 *)
  (Array : sig
    type t
    type elt = bool
    type key
    type size
    (* falseで初期化された配列を作成する *)
    val make : size -> t
    val get : t -> key -> elt
    val set : t -> key -> elt -> unit
  end)
: sig
  type vertex = Array.key
  type vertices = Array.size
  
  (* トポロジカルソート *)
  val sort :
    (* 頂点のリスト *)
    (module List with type elt = vertex and type t = 'l) ->
    (* 有向グラフ *)
    (module UnweightedDirectedGraph
      with type Vertex.t = vertex
       and type Vertex.set = vertices) ->
    (* 頂点をトポロジカルソートしたリスト *)
    'l

  (* 強連結成分分解 *)
  val scc :
    (* 頂点のリスト *)
    (module List with type elt = vertex and type t = 'l) ->
    (* 頂点のリストのリスト *)
    (module List with type elt = 'l and type t = 'll) ->
    (* 有向グラフ *)
    (module UnweightedDirectedGraph
      with type Vertex.t = vertex
       and type Vertex.set = vertices) ->
    'll
end
