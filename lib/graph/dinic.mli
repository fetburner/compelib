module F
  (* 流量 *)
  (Flow : sig
    type t
    val inf : t
    val zero : t
    val ( + ) : t -> t -> t
    val ( - ) : t -> t -> t
    val compare : t -> t -> int
  end) :
sig
  type 'a church_list = { fold : 'b. ('a -> 'b -> 'b) -> 'b -> 'b }

  val max_flow :
    (* 頂点の数n *)
    int ->
    (* 各辺と逆辺の容量のリスト
       頂点は0からn-1までの整数でなくてはならない *)
    (int * int * Flow.t * Flow.t) church_list ->
    (* 始点 *)
    int ->
    (* 終点 *)
    int ->
    (* 最大フロー *)
    Flow.t *
    (* 最大フローを流した際の各辺の流量 *)
    (int * int * Flow.t) church_list
end
