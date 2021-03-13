module type WeightedDirectedGraph = sig
  module Distance : sig
    type t
    val inf : t (* オーバーフローの恐れはないので，max_intとか突っ込んでも良い *)
    val zero : t
    val neg_inf : t  (* オーバーフローの恐れはないので，min_intとか突っ込んでも良い *)
    val compare : t -> t -> int
  end
  module Vertex : sig
    type t
    val cardinal : int (* 頂点数 *)
    (* 現在の頂点への距離を取得 *)
    val get_distance : t -> Distance.t
    (* 現在の頂点への距離を代入 *)
    val set_distance : t -> Distance.t -> unit
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

  val shortest_path :
    (module WeightedDirectedGraph
      with type Vertex.t = 'vertex
       and type Distance.t = 'distance) ->
    (* 始点 *)
    'vertex ->
    (* 頂点を受け取り，そこまでの距離を返す関数
       頂点に到達するパスが無ければ inf を返し，
       そこに到達するまでに負閉路があれば neg_inf を返す *)
    ('vertex -> 'distance)
