let fib = Compelib.Memo.memoize 10 @@ fun fib n ->
  if n <= 1 then n else fib (n - 1) + fib (n - 2)

let%test _ = List.init 10 fib = [0; 1; 1; 2; 3; 5; 8; 13; 21; 34]

let routes = Compelib.Memo.memoize 25 @@ fun routes (n, m) ->
  if n = 0 || m = 0
  then 1
  else routes (n - 1, m) + routes (n, m - 1)

let%test _ = 
  [[1; 1; 1; 1; 1];
   [1; 2; 3; 4; 5];
   [1; 3; 6; 10; 15];
   [1; 4; 10; 20; 35];
   [1; 5; 15; 35; 70]]

(* sample code *)
let ( +^ ) x y = (x + y) mod 1000000007

let fib = Compelib.Memo.memoize_cps 1000000 @@ fun fib n k ->
  if n <= 1 then k 1
  else fib (n - 1) @@ fun x -> fib (n - 2) @@ fun y -> k @@ x +^ y

let%test_unit _ = ignore @@ List.init 1000000 @@ fun x -> fib x @@ fun y -> y
