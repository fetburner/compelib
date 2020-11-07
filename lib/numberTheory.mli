val gcd : int -> int -> int
val lcm : int -> int -> int

type 'a church_list = { fold : 'b. ('a -> 'b -> 'b) -> 'b -> 'b }

val totient : int -> int
val divisors : int -> int church_list
val factorize : int -> (int * int) church_list

(* エラトステネスのふるい *)
val sieve :
  (* 素数かどうかを判定する自然数の上界 *)
  int ->
  (* 素数ならtrueを返す関数 *)
  (int -> bool)
