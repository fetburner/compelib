module F
  (Weight : sig
    type t
    val compare : t -> t -> int
  end) :
sig
  type 'a church_list = { fold : 'b. ('a -> 'b -> 'b) -> 'b -> 'b }

  (*
   * HashtblやMapを用いているとプリム法と大差ない計算時間になってしまうため，
   * より汎用的な実装が欲しければそちらを当たること
   *)
  val kruskal :
    (* 頂点の数n *)
    int ->
    (* 辺のリスト *)
    (* 頂点は0からn-1までの整数でなくてはならない
       ソートにList.sortを使っているせいでデータ構造が固定されており，あまり良くない… *)
    (int * int * Weight.t) list ->
    (* 最小全域木に含まれる辺のリスト
       畳み込む関数と初期値まで与えられて初めて計算を開始するので，
       最小全域木に対して色々な操作をしたい場合は，
       一度リストにして覚えておくとよい．
       最小全域木の重さの和だけ欲しいとか一回しか使わない場合は，
       直接畳み込む関数と初期値を与えるとdeforestationになっていい感じ． *)
    (int * int * Weight.t) church_list
end =
struct
  module UF = UnionFind.Make (struct
    type t = unit
    let union _ _ = ()
  end)

  type 'a church_list = { fold : 'b. ('a -> 'b -> 'b) -> 'b -> 'b }

  let kruskal n es =
    { fold = fun f init ->
      let uf = Array.init n @@ fun _ -> UF.make () in
      List.fold_left (fun acc (u, v, w) ->
        if UF.equal uf.(u) uf.(v)
        then acc
        else (UF.unite uf.(u) uf.(v); f (u, v, w) acc)) init @@
      List.sort (fun (_, _, w) (_, _, w') -> Weight.compare w w') es }
end
