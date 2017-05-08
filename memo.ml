(* Represents subset of 'a is finite (i.e. there exists a bijection to { 0, 1, ... , card - 1 }) *)
type 'a finite = { card : int; f_bij : 'a -> int; f_bij_inv : int -> 'a }

(* { x | src <= x < dst } is finite *)
let interval ~src ~dst =
  assert (src <= dst);
  let n = dst - src in
  { card = n;
    f_bij = (fun x ->
      assert (src <= x && x < dst);
      x - src);
    f_bij_inv = (fun x ->
      assert (0 <= x && x < n);
      x + src) }

(* If A and B are finite, A * B is also finite *)
let pair a b =
  { card = a.card * b.card;
    f_bij = (fun (x, y) -> a.f_bij x + b.f_bij y * a.card);
    f_bij_inv = (fun x -> (a.f_bij_inv (x mod a.card), b.f_bij_inv (x / a.card))) }

let memoize { card; f_bij; f_bij_inv } f =
  let dp = Array.make card @@ lazy (raise Not_found) in
  let get x = Lazy.force dp.(f_bij x) in
  Array.iteri (fun i _ -> dp.(i) <- lazy (f get @@ f_bij_inv i)) dp;
  get

(* naive unary function *)
let rec fib n =
  if n <= 1 then n else fib (n - 1) + fib (n - 2);;
(* val fib : int -> int = <fun> *)
Array.init 10 fib;;
(* - : int array = [|0; 1; 1; 2; 3; 5; 8; 13; 21; 34|] *)
fib 50;;
(* ^CInterrupted. *)

(* memoized unary function *)
let fib50 = memoize (interval ~src:0 ~dst:50) (fun fib n ->
  if n <= 1 then n else fib (n - 1) + fib (n - 2));;
(* val fib50 : int -> int = <fun> *)
Array.init 10 fib50;;
(* - : int array = [|0; 1; 1; 2; 3; 5; 8; 13; 21; 34|] *)
fib50 49;;
(* - : int = 7778742049 *)
fib50 50;;
(* Exception: Assert_failure ("//toplevel//", 131, 6). *)

(* naive binary function *)
let rec routes n m =
  if n = 0 || m = 0 then 1
  else routes (n - 1) m + routes n (m - 1)
(* val routes : int -> int -> int = <fun> *)
Array.init 5 (fun i -> Array.init 5 (routes i))
(* - : int array array =
[|[|1; 1; 1; 1; 1|]; [|1; 2; 3; 4; 5|]; [|1; 3; 6; 10; 15|];
  [|1; 4; 10; 20; 35|]; [|1; 5; 15; 35; 70|]|] *)
routes 29 29;;
(* ^CInterrupted. *)

(* memoized binary function *)
let routes3030 = memoize (pair (interval ~src:0 ~dst:30) (interval ~src:0 ~dst:30)) (fun routes (n, m) ->
  if n = 0 || m = 0 then 1
  else routes (n - 1, m) + routes (n, m - 1));;
(* val routes3030 : int -> int -> int = <fun> *)
Array.init 5 (fun i -> Array.init 5 (fun j -> routes3030 (i, j)))
(* - : int array array =
[|[|1; 1; 1; 1; 1|]; [|1; 2; 3; 4; 5|]; [|1; 3; 6; 10; 15|];
  [|1; 4; 10; 20; 35|]; [|1; 5; 15; 35; 70|]|] *)
routes3030 (29, 29);;
(* - : int = 30067266499541040 *)
routes3030 (10, 30);;
(* Exception: Assert_failure ("//toplevel//", 131, 6). *)
routes3030 (30, 10);;
(* Exception: Assert_failure ("//toplevel//", 131, 6). *)
