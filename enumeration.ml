let rec select acc trace = function
  | [] -> acc
  | x :: xs -> select ((trace, x, xs) :: acc) (x :: trace) xs
let select xs = select [] [] xs

let rec perm n xs =
  match n, xs with
  | 0, _ -> [[]]
  | _, [] -> []
  | n, xs ->
      List.concat @@
      List.map (fun (xs, x, ys) ->
        List.map (List.cons x) @@
        perm (n - 1) @@
        List.rev_append xs ys) @@
      select xs
(* 与えられたリストからn要素を選ぶ順列を列挙 *)
let perm : int -> 'a list -> 'a list list = fun n xs -> perm n @@ List.rev xs

let rec comb acc xs n ys =
  match n, ys with
  | 0, _ -> xs :: acc
  | _, [] -> acc
  | n, y :: ys -> comb (comb acc xs n ys) (y :: xs) (n - 1) ys
(* 与えられたリストからn要素を選ぶ組み合わせを列挙 *)
let comb : int -> 'a list -> 'a list list = fun n xs ->
  List.rev @@ comb [] [] n @@ List.rev xs

let rec repcomb acc xs n ys =
  match n, ys with
  | 0, _ -> xs :: acc
  | _, [] -> acc
  | n, y :: ys' -> repcomb (repcomb acc xs n ys') (y :: xs) (n - 1) ys
(* 与えられたリストから重複を許してn要素を選ぶ組み合わせを列挙 *)
let repcomb : int -> 'a list -> 'a list list = fun n xs ->
  List.rev @@ repcomb [] [] n @@ List.rev xs;;

(* sample code *)
perm 2 [1; 2; 3; 4];;
perm 0 [1; 2; 3];;
perm 3 [1; 2; 3];;
perm 4 [1; 2; 3];;

comb 2 [1; 2; 3];;
comb 3 [1; 2; 3];;
comb 0 [1; 2; 3];;
comb 4 [1; 2; 3];;

repcomb 2 [1; 2; 3];;
repcomb 3 [1; 2; 3];;
repcomb 0 [1; 2; 3];;
repcomb 4 [1; 2; 3];;

(* 添字の違う要素は区別される *)
perm 2 [1; 1; 2];;
comb 2 [1; 1; 2];;
repcomb 2 [1; 1; 2];;

