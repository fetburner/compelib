type 'a mutable_list = Nil | Cons of 'a * 'a mutable_list ref

(* 与えられたリストからn要素を選ぶ順列を列挙 *)
let perm : int -> 'a list -> 'a list list = fun n xs ->
  let head = ref Nil in
  (* xsを変更可能なリストに変換 *)
  ignore (List.fold_left (fun tail x ->
    let tail' = ref Nil in
    tail := Cons (x, tail'); tail') head xs);
  let rec perm_aux ys n ptr acc =
    if n <= 0
    then ys :: acc
    else match !ptr with
         | Nil -> acc
         | (Cons (x, next)) as here ->
             let acc = perm_aux ys n next acc in
             ptr := !next; (* リストからconsセルhereを削除 *)
             (* consセルを削除したリストから，n-1要素を選ぶ *)
             let acc = perm_aux (x :: ys) (n - 1) head acc in
             ptr := here; (* consセルを戻す *)
             acc in
  perm_aux [] n head []

let rec comb acc xs n ys =
  match n, ys with
  | 0, _ -> xs :: acc
  | _, [] -> acc
  | n, y :: ys -> comb (comb acc xs n ys) (y :: xs) (n - 1) ys
(* 与えられたリストからn要素を選ぶ組み合わせを列挙 *)
let comb : int -> 'a list -> 'a list list = fun n -> comb [] [] n

let rec repcomb acc xs n ys =
  match n, ys with
  | 0, _ -> xs :: acc
  | _, [] -> acc
  | n, y :: ys' -> repcomb (repcomb acc xs n ys') (y :: xs) (n - 1) ys
(* 与えられたリストから重複を許してn要素を選ぶ組み合わせを列挙 *)
let repcomb : int -> 'a list -> 'a list list = fun n -> repcomb [] [] n;;

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

