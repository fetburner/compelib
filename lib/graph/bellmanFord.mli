module type WeightedDirectedGraph = sig
  module Vertex : sig
    type t
    type set
    val universe : set (* 頂点の全体集合 *)
    val cardinal_universe : int (* 頂点数 *)
    val compare : t -> t -> int
  end
  module Distance : sig
    type t
    val inf : t (* オーバーフローの恐れはないので，max_intとか突っ込んでも良い *)
    val zero : t
    val neg_inf : t  (* オーバーフローの恐れはないので，min_intとか突っ込んでも良い *)
    val compare : t -> t -> int
  end
  module Edge : sig
    type t
    val universe : t list (* グラフに含まれる辺のリスト *)
    val source : t -> Vertex.t
    val destination : t -> Vertex.t
    (* 辺を通った際のコストを加算する関数 *)
    val add_weight : t -> Distance.t -> Distance.t
  end
end

module F
  (* 頂点を添字，経路長を要素とした配列の実装 *)
  (Array : sig
    type t
    type key
    type elt
    type size
    (* 無限大 で初期化された配列を作る *)
    val make : size -> t
    val get : t -> key -> elt
    val set : t -> key -> elt -> unit
  end) :
sig
  type vertex = Array.key
  type distance = Array.elt
  type vertices = Array.size

  val shortest_path :
    (module WeightedDirectedGraph
      with type Vertex.t = vertex
       and type Vertex.set = vertices
       and type Distance.t = distance) ->
    (* 始点 *)
    vertex ->
    (* 頂点を受け取り，そこまでの距離を返す関数
       頂点に到達するパスが無ければ inf を返し，
       そこに到達するまでに負閉路があれば neg_inf を返す *)
    (vertex -> distance)
end
