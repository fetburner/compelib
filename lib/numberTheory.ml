(* 最大公約数 *)
let rec gcd n m =
  if m = 0 then n else gcd m (n mod m)

(* 最小公倍数 *)
let lcm n m = n / gcd n m * m

let factorize n =
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
    then f n 1 acc
    else iter_div p 0 n @@ fun c ->
           factorize f (if c <= 0 then acc else f p c acc) (p + 1) in
  fun f acc -> factorize f acc 2 n

(* トーシェント関数φ *)
let totient n = Fun.flip (factorize n) n @@ fun p _ n -> n * (p - 1) / p

(* 昇順にソートされた約数のリスト *)
let divisors n f acc =
  let rec divisors acc acc' i =
    match compare n (i * i) with
    | -1 -> List.fold_left (Fun.flip f) acc' acc
    | 0  -> List.fold_left (Fun.flip f) (f i acc') acc
    | 1  -> (if 0 < n mod i then divisors acc acc' else divisors (i :: acc) (f (n / i) acc')) (i + 1)
    | _  -> raise Not_found in
  divisors [] acc 1

let sieve n =
  (* 今ふるおうとしている整数 *)
  let p = ref 3 in
  (* pがどのインデックスに対応するか *)
  let i = ref 0 in
  (* pの2乗 *)
  let pp = ref 9 in
  (* p^2がどのインデックスに対応するか *)
  let ii = ref 3 in
  (*
   * 2の倍数は無視し，3, 5, 7, 9 ... が格納されているとみなす
   * よって添字から格納されている数へ変換する式は 2i + 3
   * 格納されている数から添字へ変換する式は (x - 3) / 2
   *)
  let m = (n lsr 1) - 1 in
  let a = Array.make m true in
  let rec sieve_aux k =
    if k < !pp
    then a.((k - 3) lsr 1)
    else begin
      let rec mark i =
        if i < m then (a.(i) <- false; mark (i + !p)) in
      (if a.(!i) then mark !ii);
      incr i;
      p := !p + 2;
      pp := !pp + (!p lsl 1);
      ii := !ii + (!p lsl 1) - 2;
      sieve_aux k
    end in
  function
    | k when k <= 1 -> false
    | 2 -> true
    | k -> k land 1 = 1 && sieve_aux k
