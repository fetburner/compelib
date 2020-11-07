module F
  (Vertex : sig
    type t
    (* グラフに含まれる頂点の集合 *)
    type u
    (* グラフに含まれる頂点の集合に対するfold *)
    val fold : (t -> 'a -> 'a) -> u -> 'a -> 'a
  end)
  (* 頂点を添字，真偽値を要素とした配列の実装 *)
  (Array : sig
    type t
    type elt = bool
    type key = Vertex.t
    type size = Vertex.u
    (* falseで初期化された配列を作成する *)
    val make : size -> t
    val get : t -> key -> elt
    val set : t -> key -> elt -> unit
  end)
: sig
  type vertex = Vertex.t
  type vertices = Vertex.u
  
  (* トポロジカルソート *)
  val sort :
    (* グラフに含まれる頂点の集合 *)
    vertices ->
    (* 隣接リスト *)
    (vertex -> vertex list) ->
    vertex list

  (* 強連結成分分解 *)
  val scc :
    (* グラフに含まれる頂点の集合 *)
    vertices ->
    (* 隣接リスト *)
    (vertex -> vertex list) ->
    vertex list list
end
