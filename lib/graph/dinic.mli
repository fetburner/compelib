module F
  (Vertex : sig
    type t
    val equal : t -> t -> bool
  end)
  (* 流量 *)
  (Flow : sig
    type t
    val inf : t
    val zero : t
    val ( + ) : t -> t -> t
    val ( - ) : t -> t -> t
    val compare : t -> t -> int
  end)
  (Array : sig
    type 'a t
    type key = Vertex.t
    type size
    (* [make n x] : 全要素が x で初期化された大きさ n の配列を作る *)
    val make : size -> 'a -> 'a t
    val get : 'a t -> key -> 'a
    val set : 'a t -> key -> 'a -> unit
    (* [blit s t] : 配列 s の全要素を配列 t にコピーする *)
    val blit : 'a t -> 'a t -> unit
  end)
: sig
  type flow = Flow.t
  type vertex = Vertex.t
  type capacity = Flow.t
  type vertices = Array.size

  val max_flow :
    (* 頂点の数 *)
    vertices ->
    (* 辺を登録する関数を受け取って，フローグラフに含まれる辺を登録する関数
       辺を登録する関数は，辺の頂点と容量を受け取ると，最大フローを流した時にその辺を流れる流量を返す関数を返してくれる *)
    ((src:vertex -> dst:vertex -> capacity -> (unit -> flow)) -> unit) ->
    (* 始点 *)
    src:vertex ->
    (* 終点 *)
    dst:vertex ->
    (* 最大フローを流す
       この時，各辺の流量が更新される *)
    flow
end
