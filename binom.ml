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
  fun k -> binom_memo_aux @@ min k @@ n - k;;

(* sample *)

(* その昔AtCoderのOCaml処理系のintが31ビットしか無かった頃，
   よく答えがintの範囲に収まらなくてint64で計算をしたものだなぁ（しみじみ） *)
let open Int64 in binom ~of_int ~mul ~div 4 2;;
let open Int64 in binom ~of_int ~mul ~div 10 3;;
let open Int64 in binom_memo ~of_int ~mul ~div 4 2;;
let open Int64 in binom_memo ~of_int ~mul ~div 10 3;;

(* クソデカ素数で割った余りを求めたいとき *)
let rec power ( * ) e m n =
  if n <= 0 then e
  else power ( * ) (if n land 1 = 0 then e else m * e) (m * m) (n lsr 1)

let m = 998244353
let ( *^ ) x y = (x * y) mod m
let ( /^ ) x y = power ( *^ ) x y (m - 2);; (* フェルマーの小定理 *)

binom ~of_int:Fun.id ~mul:( *^ ) ~div:( /^ ) 4 2;;
binom ~of_int:Fun.id ~mul:( *^ ) ~div:( /^ ) 10 3;;
binom ~of_int:Fun.id ~mul:( *^ ) ~div:( /^ ) 100000 1000;;
binom_memo ~of_int:Fun.id ~mul:( *^ ) ~div:( /^ ) 4 2;;
binom_memo ~of_int:Fun.id ~mul:( *^ ) ~div:( /^ ) 10 3;;
binom_memo ~of_int:Fun.id ~mul:( *^ ) ~div:( /^ ) 100000 1000;;

(* 前者はO(n^2)だが，メモ化のお陰で後者はO(n)になる *)
let a = Array.init 1001 (binom ~of_int:Fun.id ~mul:( *^ ) ~div:( /^ ) 1000)
let a' = Array.init 1001 (binom_memo ~of_int:Fun.id ~mul:( *^ ) ~div:( /^ ) 1000);;
(* もちろん計算結果は一致 *)
List.for_all Fun.id @@ List.init 1001 @@ fun i -> a.(i) = a'.(i);;
