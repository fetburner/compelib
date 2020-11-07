module F
  (Weight : sig
    type t
    val inf : t (* オーバーフローの恐れはないので，max_intとか突っ込んでも良い *)
    val zero : t
    val neg_inf : t (* オーバーフローの恐れはないので，min_intとか突っ込んでも良い *)
    val ( + ) : t -> t -> t
    val compare : t -> t -> int
  end)
: sig
  type 'a church_list = { fold : 'b. ('a -> 'b -> 'b) -> 'b -> 'b }

  (* あまり行儀が良くない関数達 *)

  (* 隣接行列を作る関数 *)
  val make_adjmatrix :
    (* 頂点の数n *)
    int ->
    (* 辺のリスト
       頂点は0からn-1までの整数でなくてはならない *)
    (int * int * Weight.t) church_list ->
    (* 隣接行列 *)
    Weight.t array array

  val raw_warshall_floyd :
    (* 頂点の数n *)
    int ->
    (* 隣接行列 *)
    Weight.t array array ->
    (* 隣接行列に上書きして，各頂点間の最短距離を格納する *)
    unit

  (* 行儀の良い関数 *)

  val warshall_floyd :
    (* 頂点の数n *)
    int ->
    (* 辺のリスト
       頂点は0からn-1までの整数でなくてはならない *)
    (int * int * Weight.t) church_list ->
    (* 辿り着けなければinf，負閉路を含む経路があればneg_infを返す関数 *)
    (int -> int -> Weight.t)
end
