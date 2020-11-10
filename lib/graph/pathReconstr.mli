module F
  (* 辺の重み *)
  (Weight : sig
    type t
    val equal : t -> t -> bool
  end)
  (* 辺 *)
  (Edge : sig
    type t
    type vertex
    type weight = Weight.t
    val source : t -> vertex
    val add_weight : t -> weight -> weight
  end)
  (* 経路の``集合'' *)
  (Path : sig
    type t
    type edge = Edge.t
    (* 長さ0の経路だけからなる集合 *)
    val nil : t
    (* 空集合 *)
    val empty : t
    (* 和集合
       最短経路を一つだけ求めれば良い場合は
       let join = Fun.const
       とすればよい *)
    val join : t -> (unit -> t) -> t
    (* 集合に含まれる全ての経路の後端に辺を追加する *)
    val snoc : t -> edge -> t
  end)
  (Thunk : sig
    type t
    type elt = Path.t
    val value : elt -> t
    val running : t
    val case : t -> value:(elt -> 'a) -> pending:(unit -> 'a) -> running:(unit -> 'a) -> 'a
  end)
  (* 頂点を添字，Thunk.t を要素とした配列の実装 *)
  (Array : sig
    type t
    type key = Edge.vertex
    type elt = Thunk.t
    type size

    (* 全ての頂点について Thunk.pending で初期化された配列を作る *)
    val make : size -> t
    val get : t -> key -> elt
    val set : t -> key -> elt -> unit
  end)
: sig
  type edge = Edge.t
  type path = Path.t
  type weight = Edge.weight
  type vertex = Edge.vertex
  type vertices = Array.size

  exception ZeroCycle

  (* 最短経路の復元を行う
     0辺だけの閉路があると ZeroCycle を投げる *)
  val path_reconstruction :
    (* グラフに含まれる頂点の集合 *)
    vertices ->
    (* 最短経路を求めたいグラフの，ある頂点``に''伸びる辺のストリーム *)
    (vertex -> edge Seq.t) ->
    (* 始点 *)
    vertex ->
    (* ある頂点への始点からの最短距離を返す関数 *)
    (vertex -> weight) ->
    (* ある頂点への始点からの最短経路の集合を返す関数
       この関数を覚えておけば，呼び出しごとの途中までの計算結果がシェアされる *)
    (vertex -> path)
end
