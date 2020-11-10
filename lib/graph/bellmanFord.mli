module F
  (Vertex : sig
    type t
    val compare : t -> t -> int
  end)
  (Weight : sig
    type t
    val inf : t (* オーバーフローの恐れはないので，max_intとか突っ込んでも良い *)
    val zero : t
    val neg_inf : t (* オーバーフローの恐れはないので，min_intとか突っ込んでも良い *)
    val ( + ) : t -> t -> t
    val compare : t -> t -> int
  end)
  (* 頂点を添字，経路長を要素とした配列の実装 *)
  (Array : sig
    type t
    type key = Vertex.t
    type elt = Weight.t
    type size
    (* Weight.inf で初期化された配列を作る *)
    val make : size -> t
    val get : t -> key -> elt
    val set : t -> key -> elt -> unit
    val size_to_int : size -> int
  end) :
sig
  type vertex = Vertex.t
  type weight = Weight.t
  type vertices = Array.size

  val shortest_path :
    (* 頂点数n *)
    vertices ->
    (* 辺のリスト
       緩和のスケジューリングのためにデータ構造が固定されている
       重さは辺を通った際のコストを加算する関数として与える *)
    ((vertex * vertex * (Weight.t -> Weight.t)) list) ->
    (* 始点 *)
    vertex ->
    (* 頂点を受け取り，そこまでの距離を返す関数
       頂点に到達するパスが無ければinfを返し，
       そこに到達するまでに負閉路があればneg_infを返す *)
    (vertex -> Weight.t)
end
