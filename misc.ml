(* fold f init n = f^n(init) *)
let rec foldn f init = function
  | 0 -> init
  | n -> foldn f (f init) (n - 1)

let rec gcd n m =
  if m = 0 then n
  else gcd m (n mod m)

let lcm n m = n / gcd n m * m

let rec comb n = function
  | 0 -> 1
  | r -> comb (n - 1) (r - 1) * n / r
let comb n r =
  if 2 * r <= n then comb n r
  else comb n (n - r)

let rec take n = function
  | [] -> []
  | x :: xs ->
      if n = 0 then []
      else x :: take (n - 1) xs

let rec fold_tournament dir f = function
  | [x] -> x
  | x :: xs ->
      List.fold_left (fun (acc, prev) x ->
        match prev with
        | None -> (acc, Some x)
        | Some y -> ((if dir then f y x else f x y) :: acc, None)) ([], Some x) xs
      |> (function
          | (acc, None) -> acc
          | (acc, Some x) -> x :: acc)
      |> fold_tournament (not dir) f
(* fold_tournament ( * ) [x1; x2; x3; x4; x5; x6; x7; x8 ... ] = (... (((x1 * x2) * (x3 * x4)) * ((x5 * x6) * (x7 * x8))) ...) * ( ... ) *)
let fold_tournament f xs = fold_tournament true f xs

let array_of_string s = Array.init (String.length s) (fun i -> s.[i])

exception Exodus
let call_cc f =
  let return = ref (fun () -> raise Not_found) in
  try f (fun x -> (return := fun () -> x); raise Exodus)
  with Exodus -> !return ()
