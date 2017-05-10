fun memoize { hash, eq } (n, f) =
  let
    val dp = HashTable.mkTable (hash, eq) (n, Domain)
    fun get x =
      HashTable.lookup dp x
      handle Domain =>
        let val result = f (get, x) in
          HashTable.insert dp (x, result);
          result
        end
  in get end

(* naive unary function *)
fun fib 0 = 0
  | fib 1 = 1
  | fib n = fib (n - 1) + fib (n - 2);
(* val fib = fn : int -> int *)
List.tabulate (10, fib);
(* val it = [0,1,1,2,3,5,8,13,21,34] : int list *)
fib 44;
(* ^CInterrupt *)

(* memoized unary function *)
val fib45 = memoize { hash = Word.fromInt, eq = op= } (45, fn
    (_, 0) => 0
  | (_, 1) => 1
  | (fib, n) => fib (n - 1) + fib (n - 2));
(* val fib45 = fn : int -> int *)
List.tabulate (10, fib45);
(* val it = [0,1,1,2,3,5,8,13,21,34] : int list *)
fib45 44;
(* val it = 701408733 : int *)
fib45 45;
(* uncaught exception Overflow [overflow]
  raised at: <file stdIn> *)

(* naive binary function *)
fun routes 0 _ = 1
  | routes _ 0 = 1
  | routes n m = routes (n - 1) m + routes n (m - 1)
(* val routes = fn : int -> int -> int = <fun> *)
List.tabulate (5, fn i => List.tabulate (5, fn j => routes i j));
(* val it = [[1,1,1,1,1],[1,2,3,4,5],[1,3,6,10,15],[1,4,10,20,35],[1,5,15,35,70]] *)
routes 16 16;
(* ^CInterrupt *)

(* memoized binary function *)
val routes3030 = memoize
  { eq = op=, hash = fn (n, m) => 0w30 * Word.fromInt n + Word.fromInt m }
  (900, fn (_, (0, _)) => 1
    | (_, (_, 0)) => 1
    | (routes, (i, j)) => routes (i - 1, j) + routes (i, j - 1));
(* val routes3030 = fn : int * int -> int *)
List.tabulate (5, fn i => List.tabulate (5, fn j => routes3030 (i, j)));
(* val it = [[1,1,1,1,1],[1,2,3,4,5],[1,3,6,10,15],[1,4,10,20,35],[1,5,15,35,70]] *)
routes3030 (16, 16);
(* val it = 601080390 : int *)
