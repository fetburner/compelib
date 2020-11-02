(* ストリームの先頭n要素をマージソートし，ソート前の転倒数を計算する
   ストリームの要素数がnに満たない場合は適当な例外を投げる *)
let sort_count :
  (* 要素の比較関数（compareみたいなの） *)
  ('a -> 'a -> int) ->
  (* 先頭から何個までの要素をソートするか *)
  int ->
  (* ソートする要素の入ったストリーム *)
  'a Seq.t ->
  (* 継続渡しスタイル *)
  ('a Seq.t -> (* 先頭からn要素を取り除いたストリーム *)
   int -> (* 転倒数 *)
   'a list -> (* ソート結果 *)
   'b) -> 'b
= fun cmp ->
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
    match n with
    | 0 -> k xs invs []
    | 1 ->
        let Seq.Cons (x, xs) = xs () in
        k xs invs [x]
    | _ ->
        sort_count_rev invs ((n + 1) lsr 1) xs @@ fun xs invs ys ->
          sort_count_rev invs (n lsr 1) xs @@ fun xs invs zs ->
            rev_merge_count_rev ys zs (n lsr 1) invs [] @@ k xs
  and sort_count_rev invs n xs k =
    match n with
    | 0 -> k xs invs []
    | 1 ->
        let Seq.Cons (x, xs) = xs () in
        k xs invs [x]
    | _ ->
        sort_count invs (n lsr 1) xs @@ fun xs invs ys ->
          sort_count invs ((n + 1) lsr 1) xs @@ fun xs invs zs ->
            rev_merge_count ys (n lsr 1) zs invs [] @@ k xs in
  sort_count 0;;

(* sample *)
sort_count compare 7 (List.to_seq [3; 1; 4; 1; 5; 9; 2]) @@ fun _ invs xs -> invs, xs;;

let measure f =
  let start = Sys.time () in
  f ();
  Sys.time () -. start;;

(* 標準ライブラリのList.sortよりちょっと遅い程度 *)
let l = List.init 1000000 @@ fun _ -> Random.bits ();;

sort_count compare 1000000 (List.to_seq l) @@ fun _ _ -> ( = ) @@ List.sort compare l;;
measure @@ fun () -> ignore @@ List.sort compare l;;
measure @@ fun () -> ignore @@ sort_count compare 1000000 (List.to_seq l) @@ fun _ _ _ -> ();;