module M = Compelib.Memo.F (struct
  type t = int array
  type key = int
  type elt = int
  type size = int
  let make = Fun.flip Array.make (-1)
  let get a i =
    let x = a.(i) in
    if x < 0 then None else Some x
  let set = Array.set
end)

let fib = M.memoize 10 @@ fun fib n ->
  if n <= 1 then n else fib (n - 1) + fib (n - 2)

let%test _ = List.init 10 fib = [0; 1; 1; 2; 3; 5; 8; 13; 21; 34]

module N = Compelib.Memo.F (struct
  type t = int array array
  type key = int * int
  type elt = int
  type size = int * int
  let make (n, m) = Array.make_matrix n m (-1)
  let get a (i, j) =
    let x = a.(i).(j) in
    if x < 0 then None else Some x
  let set a (i, j) x = a.(i).(j) <- x
end)

let routes = N.memoize (5, 5) @@ fun routes (n, m) ->
  if n = 0 || m = 0
  then 1
  else routes (n - 1, m) + routes (n, m - 1)

let%test _ =
  List.init 5 (fun i -> List.init 5 (fun j -> routes (i, j))) =
  [[1; 1; 1; 1; 1];
   [1; 2; 3; 4; 5];
   [1; 3; 6; 10; 15];
   [1; 4; 10; 20; 35];
   [1; 5; 15; 35; 70]]

let ( +^ ) x y = (x + y) mod 1000000007

let fib = M.memoize_cps 1000000 @@ fun fib n k ->
  if n <= 1 then k 1
  else fib (n - 1) @@ fun x -> fib (n - 2) @@ fun y -> k @@ x +^ y

let%test_unit _ = ignore @@ List.init 1000000 @@ fun x -> fib x @@ fun y -> y
