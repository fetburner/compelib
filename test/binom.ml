(* その昔AtCoderのOCaml処理系のintが31ビットしか無かった頃，
   よく答えがintの範囲に収まらなくてint64で計算をしたものだなぁ（しみじみ） *)
let%test "4C2 in int64" = let open Int64 in Compelib.Binom.binom ~of_int ~mul ~div 4 2 = 6L
let%test "10C3 in int64" = let open Int64 in Compelib.Binom.binom ~of_int ~mul ~div 10 3 = 120L
let%test "4C2 in int64 (memoized ver)" = let open Int64 in Compelib.Binom.binom_memo ~of_int ~mul ~div 4 2 = 6L
let%test "10C3 in int64 (memoized ver)" = let open Int64 in Compelib.Binom.binom_memo ~of_int ~mul ~div 10 3 = 120L

(* クソデカ素数で割った余りを求めたいとき *)
let m = 998244353
let ( *^ ) x y = (x * y) mod m
let ( /^ ) x y = Compelib.Misc.power ( *^ ) x y (m - 2) (* フェルマーの小定理 *)

let%test "4C2 mod prime" = Compelib.Binom.binom ~of_int:Fun.id ~mul:( *^ ) ~div:( /^ ) 4 2 = 6
let%test "10C3 mod prime" = Compelib.Binom.binom ~of_int:Fun.id ~mul:( *^ ) ~div:( /^ ) 10 3 = 120
let%test "100000C1000 mod prime" = 0 <= Compelib.Binom.binom ~of_int:Fun.id ~mul:( *^ ) ~div:( /^ ) 100000 1000
let%test "4C2 mod prime (memoized ver)" = Compelib.Binom.binom_memo ~of_int:Fun.id ~mul:( *^ ) ~div:( /^ ) 4 2 = 6
let%test "10C3 mod prime (memoized ver)" = Compelib.Binom.binom_memo ~of_int:Fun.id ~mul:( *^ ) ~div:( /^ ) 10 3 = 120
let%test "100000C1000 mod prime (memoized ver)" = 0 <= Compelib.Binom.binom_memo ~of_int:Fun.id ~mul:( *^ ) ~div:( /^ ) 100000 1000

(* 前者はO(n^2)だが，メモ化のお陰で後者はO(n)になる *)
let a = Array.init 1001 (Compelib.Binom.binom ~of_int:Fun.id ~mul:( *^ ) ~div:( /^ ) 1000)
let a' = Array.init 1001 (Compelib.Binom.binom_memo ~of_int:Fun.id ~mul:( *^ ) ~div:( /^ ) 1000)
(* もちろん計算結果は一致 *)
let%test "Compelib.Binom.binom consists Compelib.Binom.memo" = List.for_all Fun.id @@ List.init 1001 @@ fun i -> a.(i) = a'.(i)
