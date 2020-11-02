(* intと準同型な体の上で組み合わせの数nCkを計算する関数
   時間計算量はO(k)，空間計算量はO(1) *)
let binom :
  (* intからの準同型写像 *)
  of_int:(int -> 'a) ->
  (* 乗算 *)
  mul:('a -> 'a -> 'a) ->
  (* 除算 *)
  div:('a -> 'a -> 'a) ->
  (* 全体の数n *)
  int ->
  (* 選ぶ個数k *)
  int ->
  (* 組み合わせの数nCk *)
  'a
= fun ~of_int ~mul ~div n k ->
  let rec binom_aux nCi k i =
    if k <= i
    then nCi
    else binom_aux (div (mul nCi (of_int (n - i))) (of_int (i + 1))) k (i + 1) in
  binom_aux (of_int 1) (min k (n - k)) 0

(* 全体の数nを固定して，選ぶ個数kを変えた時の組み合わせの数nCkを
   intと準同型な体の上でメモ化して計算する関数
   nを固定してk以下の全てのk'に対してnCk'を計算する時，
   時間計算量はO(k)，空間計算量はO(n) *)
let binom_memo :
  (* intからの準同型写像 *)
  of_int:(int -> 'a) ->
  (* 乗算 *)
  mul:('a -> 'a -> 'a) ->
  (* 除算 *)
  div:('a -> 'a -> 'a) ->
  (* 全体の数n *)
  int ->
  (* 選ぶ個数kを受け取って，組み合わせの数nCkを返す関数 *)
  (int -> 'a)
= fun ~of_int ~mul ~div n ->
  let i = ref 0 in
  (* 組み合わせの数を覚えておく配列
     !i以下の要素は既に組み合わせの数をメモしている *)
  let a = Array.make (n / 2 + 1) (of_int 1) in
  (* 途中までの計算結果をメモしつつ，組み合わせの数nCkを求める
     nを部分適用した関数を覚えておけば，途中までの計算結果はシェアされる *)
  let rec binom_memo_aux k =
    if k <= !i
    then a.(k)
    else begin
      incr i;
      a.(!i) <- div (mul a.(!i - 1) (of_int (n - !i + 1))) (of_int !i);
      binom_memo_aux k
    end in
  fun k -> binom_memo_aux @@ min k @@ n - k
