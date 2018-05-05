let rec comb acc xs n ys =
  match n, ys with
  | 0, _ -> xs :: acc
  | _, [] -> acc
  | n, y :: ys -> comb (comb acc xs n ys) (y :: xs) (n - 1) ys
(* 与えられたリストからn要素を選ぶ組み合わせを列挙 *)
let comb n xs = comb [] [] n xs

(* sample code *)
comb 2 [1; 2; 3];;
comb 3 [1; 2; 3];;
comb 0 [1; 2; 3];;
comb 4 [1; 2; 3];;
comb 2 [1; 1; 2];;
