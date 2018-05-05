let rec select acc trace = function
  | [] -> acc
  | x :: xs -> select ((trace, x, xs) :: acc) (x :: trace) xs
let select xs = select [] [] xs

(* 与えられたリストからn要素を選ぶ順列 *)
let rec perm : int -> 'a list -> 'a list list = fun n xs ->
  match n, xs with
  | 0, _ -> [[]]
  | _, [] -> []
  | n, xs ->
      List.concat @@
        List.map (fun (xs, x, ys) ->
          List.map (fun xs -> x :: xs) @@
            perm (n - 1) @@ List.rev_append xs ys) @@ select xs;;

(* sample code *)
perm 2 [1; 2; 3; 4];;
perm 0 [1; 2; 3];;
perm 3 [1; 2; 3];;
perm 4 [1; 2; 3];;
perm 2 [1; 1; 2];;
