(* マージソートをしつつ，転倒数を計算する
   基本的に全て末尾再帰のため，スタックオーバーフローの心配はない *)
let sort_count :
  (* 要素の比較関数（compareみたいなの） *)
  ('a -> 'a -> int) ->
  (* リスト *)
  'a list ->
  (* 転倒数とソート結果の組 *)
  int * 'a list
= fun cmp xs ->
  (* 標準ライブラリのList.sortをCPS変換して，転倒数を計算する処理を追加したような実装 *)
  let rec rev_merge_count xs lxs ys invs acc k =
    match xs, ys with
    | _, [] -> k invs (List.rev_append xs acc)
    | [], _ -> k invs (List.rev_append ys acc)
    | x :: xs', y :: ys' ->
        if cmp x y <= 0
        then rev_merge_count xs' (lxs - 1) ys invs (x :: acc) k
        else rev_merge_count xs lxs ys' (lxs + invs) (y :: acc) k in
  let rec rev_merge_count_rev xs ys lys invs acc k =
    match xs, ys with
    | _, [] -> k invs (List.rev_append xs acc)
    | [], _ -> k invs (List.rev_append ys acc)
    | x :: xs', y :: ys' ->
        if cmp x y > 0
        then rev_merge_count_rev xs' ys lys (lys + invs) (x :: acc) k
        else rev_merge_count_rev xs ys' (lys - 1) invs (y :: acc) k in
  let rec sort_count invs n xs k =
    match n, xs with
    | 0, _ -> k xs invs []
    | 1, x :: xs -> k xs invs [x]
    | _, _ ->
        sort_count_rev invs ((n + 1) lsr 1) xs @@ fun xs invs ys ->
          sort_count_rev invs (n lsr 1) xs @@ fun xs invs zs ->
            rev_merge_count_rev ys zs (n lsr 1) invs [] @@ k xs
  and sort_count_rev invs n xs k =
    match n, xs with
    | 0, _ -> k xs invs []
    | 1, x :: xs -> k xs invs [x]
    | _, _ ->
        sort_count invs (n lsr 1) xs @@ fun xs invs ys ->
          sort_count invs ((n + 1) lsr 1) xs @@ fun xs invs zs ->
            rev_merge_count ys (n lsr 1) zs invs [] @@ k xs in
  sort_count 0 (List.length xs) xs @@ fun _ invs zs -> invs, zs;;

(* sample *)
sort_count compare [3; 1; 4; 1; 5; 9; 2]

let measure f =
  let start = Sys.time () in
  f ();
  Sys.time () -. start;;

(* 標準ライブラリのList.sortよりちょっと遅い程度 *)
let l = List.init 1000000 @@ fun _ -> Random.bits ();;

measure @@ fun () -> ignore @@ List.sort compare l;;
measure @@ fun () -> ignore @@ sort_count compare l;;
