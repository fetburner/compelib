(* Represents subset of 'a is finite (i.e. there exists a bijection to { 0, 1, ... , card - 1 }) *)
type 'a finite = { card : int, f_bij : 'a -> int, f_bij_inv : int -> 'a }

(* { x | src <= x < dst } is finite *)
fun interval { src, dst } : int finite =
  { card = dst - src,
    f_bij = (fn x => x - src),
    f_bij_inv = (fn x => x + src) }

(* If A and B are finite, A * B is also finite *)
fun pair (a : 'a finite) (b : 'b finite) : ('a * 'b) finite =
  { card = #card a * #card b,
    f_bij = (fn (x, y) => #f_bij a x + #f_bij b y * #card a),
    f_bij_inv = (fn x => (#f_bij_inv a (x mod #card a), #f_bij_inv b (x div #card a))) }

datatype 'a thunk = VALUE of 'a
                  | SUSPEND of unit -> 'a

fun memoize ({ card, f_bij, f_bij_inv } : 'a finite) f =
  let
    val dp = Array.array (card, SUSPEND (fn () => raise Domain))
    fun get x =
      let val i = f_bij x in
        case Array.sub (dp, i) of
             VALUE v => v
           | SUSPEND f =>
               let val x = f () in
                 Array.update (dp, i, VALUE x); x
               end
      end
  in
    Array.modifyi (fn (i, _) => SUSPEND (fn () => f (get, f_bij_inv i))) dp; get
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
val fib45 = memoize (interval { src = 0, dst = 45 }) (fn
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
routes 16 16;
(* ^CInterrupt *)

(* memoized binary function *)
val routes3030 = memoize
  (pair
    (interval { src = 0, dst = 30})
    (interval { src = 0, dst = 30}))
  (fn (_, (0, _)) => 1
    | (_, (_, 0)) => 1
    | (routes, (i, j)) => routes (i - 1, j) + routes (i, j - 1));
(* val routes3030 = fn : int * int -> int *)
List.tabulate (5, fn i => List.tabulate (5, fn j => routes3030 (i, j)));
(* val it = [[1,1,1,1,1],[1,2,3,4,5],[1,3,6,10,15],[1,4,10,20,35],[1,5,15,35,70]] *)
routes3030 (16, 16);
