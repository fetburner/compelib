datatype 'a thunk = VALUE of 'a
                  | SUSPEND of unit -> 'a

fun memoize (n, f) =
  let
    val dp = Array.array (n, SUSPEND (fn () => raise Domain))
    fun get i =
      case Array.sub (dp, i) of
           VALUE v => v
         | SUSPEND f =>
             let val x = f () in
               Array.update (dp, i, VALUE x); x
             end
  in
    Array.modifyi (fn (i, _) => SUSPEND (fn () => f (get, i))) dp; get
  end

fun memoize2 (n, m, f) =
  let
    val dp = Array2.array (n, m, SUSPEND (fn () => raise Domain))
    fun get (i, j) =
      case Array2.sub (dp, i, j) of
           VALUE v => v
         | SUSPEND f =>
             let val x = f () in
               Array2.update (dp, i, j, VALUE x); x
             end
  in
    Array2.modifyi
      Array2.RowMajor
      (fn (i, j, _) => SUSPEND (fn () => f (get, i, j)))
      { base = dp, row = 0, col = 0, nrows = NONE, ncols = NONE };
    get
  end

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
val fib45 = memoize (45, fn
    (_, 0) => 0
  | (_, 1) => 1
  | (fib, n) => fib (n - 1) + fib (n - 2));
(* val fib45 = fn : int -> int *)
List.tabulate (10, fib45);
(* val it = [0,1,1,2,3,5,8,13,21,34] : int list *)
fib45 44;
(* val it = 701408733 : int *)
fib45 45;
(* uncaught exception Subscript [subscript out of bounds]
  raised at: stdIn:9.12-9.21 *)

(* naive binary function *)
fun routes 0 _ = 1
  | routes _ 0 = 1
  | routes n m = routes (n - 1) m + routes n (m - 1)
(* val routes = fn : int -> int -> int = <fun> *)
List.tabulate (5, fn i => List.tabulate (5, fn j => routes i j));
(* val it = [[1,1,1,1,1],[1,2,3,4,5],[1,3,6,10,15],[1,4,10,20,35],[1,5,15,35,70]] *)
routes 17 17;;
(* ^CInterrupt *)

(* memoized binary function *)
val routes3030 = memoize2 (30, 30, fn
    (_, 0, _) => 1
  | (_, _, 0) => 1
  | (routes, i, j) => routes (i - 1, j) + routes (i, j - 1));
(* val routes3030 = fn : int * int -> int *)
List.tabulate (5, fn i => List.tabulate (5, fn j => routes3030 (i, j)));
(* val it = [[1,1,1,1,1],[1,2,3,4,5],[1,3,6,10,15],[1,4,10,20,35],[1,5,15,35,70]] *)
routes3030 (17, 17)
