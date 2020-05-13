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

  (* あまり行儀が良くない関数達 *)

  (* 隣接行列を作る関数 *)
  val make_adjmatrix :
    (* 頂点の数n *)
    int ->
    (* 辺のリスト
       頂点は0からn-1までの整数でなくてはならない *)
    (int * int * Weight.t) church_list ->
    (* 隣接行列 *)
    Weight.t array array

  val raw_warshall_floyd :
    (* 頂点の数n *)
    int ->
    (* 隣接行列 *)
    Weight.t array array ->
    (* 隣接行列に上書きして，各頂点間の最短距離を格納する *)
    unit

  (* 行儀の良い関数 *)

  val warshall_floyd :
    (* 頂点の数n *)
    int ->
    (* 辺のリスト
       頂点は0からn-1までの整数でなくてはならない *)
    (int * int * Weight.t) church_list ->
    (* 辿り着けなければinf，負閉路を含む経路があればneg_infを返す関数 *)
    (int -> int -> Weight.t)
end =
struct
  type 'a church_list = { fold : 'b. ('a -> 'b -> 'b) -> 'b -> 'b }

  let make_adjmatrix n es =
    let d = Array.make_matrix n n Weight.inf in
    for v = 0 to n - 1 do
      d.(v).(v) <- Weight.zero
    done;
    es.fold (fun (u, v, c) () ->
      (* c < d.(u).(v) *)
      if 0 < Weight.compare d.(u).(v) c
      then d.(u).(v) <- c) (); d

  (* ワーシャルフロイド法の処理の本体 *)
  let raw_warshall_floyd n d =
    for i = 0 to n - 1 do
      for j = 0 to n - 1 do
        (* 経路がない場合は更新しない *)
        if 0 < Weight.compare Weight.inf d.(j).(i) then
          for k = 0 to n - 1 do
            let open Weight in
            if 
              (* 経路がない場合は更新しない *)
              0 < Weight.compare Weight.inf d.(i).(k)
              (* d.(j).(i) + d.(i).(k) < d.(j).(k) *)
              && 0 < Weight.compare d.(j).(k) (d.(j).(i) + d.(i).(k))
            then d.(j).(k) <- d.(j).(i) + d.(i).(k)
          done
      done
    done;
    for i = 0 to n - 1 do
      (* 頂点iを通る負閉路がある *)
      if 0 < Weight.compare Weight.zero d.(i).(i) then
        for j = 0 to n - 1 do
          if 0 < Weight.compare Weight.inf d.(j).(i) then
            for k = 0 to n - 1 do
              (* 負閉路を通る経路があれば，最短距離を負の無限大で更新 *)
              if 0 < Weight.compare Weight.inf d.(i).(k) then
                d.(j).(k) <- Weight.neg_inf
          done
        done
    done

  let warshall_floyd n es =
    let d = make_adjmatrix n es in
    raw_warshall_floyd n d;
    fun u v -> try d.(u).(v) with _ -> Weight.inf
end

(* sample code *)

module Int = struct
  type t = int
  let zero = 0
  let ( + ) = ( + )
  let inf = max_int
  let neg_inf = min_int
  let compare = compare
end

module G = WeightedDirectedGraph (Int)

let d = G.warshall_floyd 5
  { G.fold = fun f -> List.fold_right f
  [ (0, 1, 4); (0, 4, 3);
    (1, 0, 4); (1, 2, 2);
    (2, 1, 2); (2, 3, 3); (2, 4, 2);
    (3, 2, 3); (3, 4, 7);
    (4, 0, 3); (4, 2, 2); (4, 3, 7) ] };;
List.init 5 @@ fun i -> List.init 5 @@ d i;;
