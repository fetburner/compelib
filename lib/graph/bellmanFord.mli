module F
  (Weight : sig
    type t
    val inf : t (* オーバーフローの恐れはないので，max_intとか突っ込んでも良い *)
    val zero : t
    val neg_inf : t (* オーバーフローの恐れはないので，min_intとか突っ込んでも良い *)
    val ( + ) : t -> t -> t
    val compare : t -> t -> int
  end) :
sig
  val bellman_ford :
    (* 頂点数n *)
    int ->
    (* 辺のリストについてのイテレータ
       頂点は0からn-1までの整数でなくてはならない *)
    ((int -> int -> Weight.t -> unit) -> unit) ->
    (* 始点 *)
    int ->
    (* 頂点を受け取り，そこまでの距離を返す関数
       頂点に到達するパスが無ければinfを返し，
       そこに到達するまでに負閉路があればneg_infを返す *)
    (int -> Weight.t)
end
