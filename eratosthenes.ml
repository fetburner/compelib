(* エラトステネスのふるい *)
let sieve :
  (* 素数かどうかを判定する自然数の上界 *)
  int ->
  (* 素数ならtrueを返す関数 *)
  (int -> bool)
  = fun n ->
    let a = Array.make n true in
    a.(0) <- false;
    a.(1) <- false;
    for p = 2 to int_of_float @@ ceil @@ sqrt @@ float_of_int n do
      if a.(p) then
        for i = p to (n - 1) / p do
          a.(p * i) <- false
        done
    done;
    Array.get a

(* sample code *)

let p = sieve 100;;
Array.init 100 (fun i -> i)
  |> Array.to_list
  |> List.filter p;;
let p = sieve 3;;
Array.init 3 p;;
let p = sieve 4;;
Array.init 4 p;;
let p = sieve 5;;
Array.init 5 p;;
let p = sieve 6;;
Array.init 6 p;;
