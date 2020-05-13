(* 最大公約数 *)
let rec gcd n m =
  if m = 0 then n else gcd m (n mod m)

(* 最小公倍数 *)
let lcm n m = n / gcd n m * m

type 'a church_list = { fold : 'b. ('a -> 'b -> 'b) -> 'b -> 'b }

let factorize :
  (* 素因数分解する整数 *)
  int ->
  (* 素因数と指数の組のリスト
     素因数について降順に出てくる *)
  (int * int) church_list
= fun n ->
  (* nをpで割れるだけ割って，割れた数c'にcを加えたものとn/p^c'を返す *)
  let rec iter_div p c n k =
    if 0 < n mod p
    then k c n
    else iter_div p (c + 1) (n / p) k in
  (* 素因数分解の処理の本体 *)
  let rec factorize f acc p n =
    if n <= 1
    then acc
    else if n < p * p
    then f (n, 1) acc
    else iter_div p 0 n @@ fun c ->
           factorize f (if c <= 0 then acc else f (p, c) acc) (p + 1) in
  { fold = fun f acc -> factorize f acc 2 n }

(* トーシェント関数φ *)
let totient n = Fun.flip (factorize n).fold n @@ fun (p, _) n -> n * (p - 1) / p

(* 昇順にソートされた約数のリスト *)
let divisors : int -> int church_list = fun n ->
  { fold = fun f acc ->
      let rec divisors acc acc' i =
        match compare n (i * i) with
        | -1 -> List.fold_left (Fun.flip f) acc' acc
        | 0  -> List.fold_left (Fun.flip f) (f i acc') acc
        | 1  -> (if 0 < n mod i then divisors acc acc' else divisors (i :: acc) (f (n / i) acc')) (i + 1) in
      divisors [] acc 1 }
