module WeightedDirectedGraph
  (Weight : sig
    type t
    val inf : t (* オーバーフローの恐れはないので，max_intとか突っ込んでも良い *)
    val zero : t
    val neg_inf : t (* オーバーフローの恐れはないので，min_intとか突っ込んでも良い *)
    val ( + ) : t -> t -> t
    val compare : t -> t -> int
  end) :
sig
  type 'a church_list = { fold : 'b. ('a -> 'b -> 'b) -> 'b -> 'b }

  val bellman_ford :
    (* 頂点数n *)
    int ->
    (* 辺のリスト
       頂点は0からn-1までの整数でなくてはならない
       2n回呼び出されるので，リストを作るのに時間がかかるならメモ化すること *)
    (int * int * Weight.t) church_list ->
    (* 始点 *)
    int ->
    (* 頂点を受け取り，そこまでの距離を返す関数
       頂点に到達するパスが無ければinfを返し，
       そこに到達するまでに負閉路があればneg_infを返す *)
    (int -> Weight.t)
end =
struct
  type 'a church_list = { fold : 'b. ('a -> 'b -> 'b) -> 'b -> 'b }

  let bellman_ford n es s =
    (* 距離を覚えるやつ *)
    let d = Array.make n Weight.inf in
    (* 経路に負閉路が含まれる頂点を覚えるやつ *)
    let neg = Array.make n false in
    d.(s) <- Weight.zero;
    (* n-1回目までの反復
       途中の反復で更新が行われなければfalseを返す *)
    let rec loop n =
      n <= 0 ||
      es.fold (fun (u, v, c) b -> (* bは更新の有無 *)
        let open Weight in
        (* 原点から到達できない頂点は更新しない *)
        0 < Weight.compare inf d.(u)
        (* c は u から v への辺の重さ
           d.(u) + c < d.(v) *)
        && 0 < Weight.compare d.(v) (d.(u) + c)
        && (d.(v) <- d.(u) + c; true) || b) false
      && loop (n - 1) in
    (* n回目以降の反復を行う関数
       途中の反復で更新が行われなければfalseを返す *)
    let rec loop' n =
      n <= 0 ||
      es.fold (fun (u, v, c) b ->
        let open Weight in
        if neg.(u) then neg.(v) <- true;
        0 < Weight.compare inf d.(u)
        && 0 < Weight.compare d.(v) (d.(u) + c)
        (* n 回目以降に変更が起こった場合，v までの経路に負閉路が含まれている *)
        && (d.(v) <- d.(u) + c; neg.(v) <- true; true) || b) false
      && loop' (n - 1) in
    if loop (n - 1) then ignore (loop' n);
    fun v -> if neg.(v) then Weight.neg_inf else d.(v)
end

(* sample code *)

module G = WeightedDirectedGraph (struct
  type t = float
  let zero = 0.
  let inf = infinity
  let neg_inf = neg_infinity
  let ( + ) = ( +. )
  let compare = compare
end)

let d = G.bellman_ford 8 { G.fold = fun f -> List.fold_right f
  [ (1, 2, 1.); (2, 3, 1.); (3, 7, 1.); (4, 5, -1.); (5, 6, -1.); (6, 4, -1.) ] } 1;;
List.init 8 d;;

let d = G.bellman_ford 8 { G.fold = fun f -> List.fold_right f
  [ (1, 2, 1.); (2, 3, 1.); (3, 7, 1.); (4, 5, -1.); (5, 6, -1.); (6, 4, -1.); (7, 4, -1.) ] } 1;;
List.init 8 d;;

let d = G.bellman_ford 8 { G.fold = fun f -> List.fold_right f
  [ (1, 2, 1.); (2, 3, 1.); (3, 7, 1.); (4, 5, -1.); (5, 6, -1.); (6, 4, -1.); (7, 4, -1.); (4, 7, 1.) ] } 1;;
List.init 8 d;;
