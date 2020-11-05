(* 座標圧縮 *)
module F
  (* 座標の型 *)
  (Coord : sig
    type t
    val compare : t -> t -> int
  end) :
sig
  val compress :
    (* 座標のリスト *)
    Coord.t list ->
    (* 座標の数，圧縮する関数と解凍する関数を返す *)
    int * (Coord.t -> int) * (int -> Coord.t)
end
