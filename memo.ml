(* n is initial size of hash table *)
let memoize n f =
  let dp = Hashtbl.create n in
  let rec get x =
    try Hashtbl.find dp x with
    | Not_found ->
        let result = f get x in
        Hashtbl.add dp x result;
        result in
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
let fib' = memoize 1000 (fun fib n ->
  if n <= 1 then n else fib (n - 1) + fib (n - 2));;
(* val fib50 : int -> int = <fun> *)
Array.init 10 fib';;
(* - : int array = [|0; 1; 1; 2; 3; 5; 8; 13; 21; 34|] *)
fib' 49;;
(* - : int = 7778742049 *)

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
let routes' = memoize 1000 (fun routes' (n, m) ->
  if n = 0 || m = 0 then 1
  else routes' (n - 1, m) + routes' (n, m - 1));;
(* val routes3030 : int -> int -> int = <fun> *)
Array.init 5 (fun i -> Array.init 5 (fun j -> routes' (i, j)));;
(* - : int array array =
[|[|1; 1; 1; 1; 1|]; [|1; 2; 3; 4; 5|]; [|1; 3; 6; 10; 15|];
  [|1; 4; 10; 20; 35|]; [|1; 5; 15; 35; 70|]|] *)
routes' (29, 29);;
(* - : int = 30067266499541040 *)

(* continuation passing style *)
let memoize n f =
  let dp = Hashtbl.create n in
  let rec get x k =
    try k @@ Hashtbl.find dp x with
    | Not_found ->
        f get x @@ fun y ->
          Hashtbl.add dp x y; k y in get

(* sample code *)
let ( +^ ) x y = (x + y) mod 1000000007

let fib = memoize 1000000 @@ fun fib n k ->
  if n <= 1 then k 1
  else fib (n - 1) @@ fun x -> fib (n - 2) @@ fun y -> k @@ x +^ y;;

(* stack overflow does not occur *)
Array.init 1000000 @@ fun x -> fib x @@ fun y -> y;;
