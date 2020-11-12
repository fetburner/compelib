module F
  (Vertices : sig
    (* グラフに含まれる頂点の集合 *)
    type t
    type vertex
    (* グラフに含まれる頂点の集合に対するfold *)
    val fold : (vertex -> 'a -> 'a) -> t -> 'a -> 'a
  end)
  (* 頂点を添字，真偽値を要素とした配列の実装 *)
  (Array : sig
    type t
    type elt = bool
    type key = Vertices.vertex
    type size = Vertices.t
    (* falseで初期化された配列を作成する *)
    val make : size -> t
    val get : t -> key -> elt
    val set : t -> key -> elt -> unit
  end)
: sig
  type vertex = Vertices.vertex
  type vertices = Vertices.t
  
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
