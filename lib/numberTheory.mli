val gcd : int -> int -> int
val lcm : int -> int -> int

val totient : int -> int
val divisors : int -> (int -> 'a -> 'a) -> 'a -> 'a
val factorize : int -> (int -> int -> 'a -> 'a) -> 'a -> 'a

(* エラトステネスのふるい *)
val sieve :
  (* 素数かどうかを判定する自然数の上界 *)
  int ->
  (* 素数ならtrueを返す関数 *)
  (int -> bool)
