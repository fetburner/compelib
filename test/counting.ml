module M64 = Compelib.Counting.F (struct
  type t = int64
  let of_int = Int64.of_int
  let ( * ) = Int64.mul
  let ( / ) = Int64.div
end)

(* その昔AtCoderのOCaml処理系のintが31ビットしか無かった頃，
   よく答えがintの範囲に収まらなくてint64で計算をしたものだなぁ（しみじみ） *)
let%test "4P2 in int64" = M64.perm 4 2 = 12L
let%test "4C2 in int64" = M64.comb 4 2 = 6L
let%test "10P3 in int64" = M64.perm 10 3 = 720L
let%test "10C3 in int64" = M64.comb 10 3 = 120L
let%test "4C2 in int64 (memoized ver)" = M64.comb_memo 4 2 = 6L
let%test "10C3 in int64 (memoized ver)" = M64.comb_memo 10 3 = 120L

(* クソデカ素数で割った余りを求めたいとき *)
module Mmod = Compelib.Counting.F (struct
  type t = int
  let m = 998244353
  let of_int = Fun.flip ( mod ) m
  let ( * ) x y = (x * y) mod m
  let ( / ) x y = Compelib.Misc.power ( * ) x y (m - 2) (* フェルマーの小定理 *)
end)

let%test "4P2 mod prime" = Mmod.perm 4 2 = 12
let%test "4C2 mod prime" = Mmod.comb 4 2 = 6
let%test "10P3 in int64" = M64.perm 10 3 = 720L
let%test "10C3 mod prime" = Mmod.comb 10 3 = 120
let%test "100000C1000 mod prime" = 0 <= Mmod.comb 100000 1000
let%test "4C2 mod prime (memoized ver)" = Mmod.comb_memo 4 2 = 6
let%test "10C3 mod prime (memoized ver)" = Mmod.comb_memo 10 3 = 120
let%test "100000C1000 mod prime (memoized ver)" = 0 <= Mmod.comb_memo 100000 1000

(* 前者はO(n^2)だが，メモ化のお陰で後者はO(n)になる *)
let a = Array.init 1001 @@ Mmod.comb 1000
let a' = Array.init 1001 @@ Mmod.comb_memo 1000
(* もちろん計算結果は一致 *)
let%test "Compelib.Counting.F.comb consists Compelib.Counting.F.memo" = List.for_all Fun.id @@ List.init 1001 @@ fun i -> a.(i) = a'.(i)
