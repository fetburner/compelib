module F
  (Int : sig
    type t
    val of_int : int -> t
    val ( * ) : t -> t -> t
    val ( / ) : t -> t -> t
  end)
: sig
  (* intと準同型な体の上で順列の数nPkを計算する関数
     時間計算量はO(k)，空間計算量はO(k) *)
  val perm :
    (* 全体の数n *)
    int ->
    (* 選ぶ個数k *)
    int ->
    (* 順列の数nCk *)
    Int.t

  (* intと準同型な体の上で組み合わせの数nCkを計算する関数
     時間計算量はO(k)，空間計算量はO(1) *)
  val comb :
    (* 全体の数n *)
    int ->
    (* 選ぶ個数k *)
    int ->
    (* 組み合わせの数nCk *)
    Int.t

  (* 全体の数nを固定して，選ぶ個数kを変えた時の組み合わせの数nCkを
     intと準同型な体の上でメモ化して計算する関数
     nを固定してk以下の全てのk'に対してnCk'を計算する時，
     時間計算量はO(k)，空間計算量はO(n) *)
  val comb_memo :
    (* 全体の数n *)
    int ->
    (* 選ぶ個数kを受け取って，組み合わせの数nCkを返す関数 *)
    (int -> Int.t)
end
