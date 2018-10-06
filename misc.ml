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

let rec floor_sqrt acc acc_x_2_x_r sq_acc_minus_z r sq_r =
  if r = 0 then acc
  else
    let sq_acc_minus_z' = sq_acc_minus_z + acc_x_2_x_r + sq_r in
    ( if sq_acc_minus_z' <= 0 then
        floor_sqrt (acc + r) ((acc_x_2_x_r lsr 1) + sq_r) sq_acc_minus_z'
      else
        floor_sqrt acc (acc_x_2_x_r lsr 1) sq_acc_minus_z) (r lsr 1) (sq_r lsr 2)
let floor_sqrt z = floor_sqrt 0 0 (~-z) (1 lsl 30) (1 lsl 60)

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

let rec factorize acc i n =
  if n <= 1 then acc
  else if n < i * i then n :: acc
  else if n mod i = 0 then factorize (i :: acc) i (n / i)
  else factorize acc (i + 1) n
let factorize n =
  match factorize [] 2 n with
  | [] -> []
  | p :: ps ->
      let (acc, p, n) = List.fold_left (fun (acc, p, n) q ->
        if p = q
        then (acc, p, n + 1)
        else ((p, n) :: acc, q, 1)) ([], p, 1) ps in
      (p, n) :: acc

let rec divisors (acc, acc') i n =
  if n < i * i
  then List.rev_append acc acc'
  else if n = i * i then List.rev_append acc (i :: acc')
  else divisors (if 0 < n mod i then (acc, acc') else (i :: acc, n / i :: acc')) (i + 1) n
let divisors = divisors ([], []) 1
