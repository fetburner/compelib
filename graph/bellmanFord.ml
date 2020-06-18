
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
       頂点は0からn-1までの整数でなくてはならない *)
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
    (* 自己辺の最小のコスト *)
    let self = Array.make n Weight.zero in
    (* 始点より終点のインデックスが大きくなる辺を集めた隣接リスト *)
    let inc = Array.make n [] in
    (* 始点より終点のインデックスが小さくなる辺を集めた隣接リスト *)
    let dec = Array.make n [] in
    es.fold (fun ((u, v, c) as e) () ->
      match compare v u with
      | 0 -> if 0 < Weight.compare self.(u) c then self.(u) <- c
      | cmp when 0 < cmp -> inc.(u) <- e :: inc.(u)
      | _ -> dec.(u) <- e :: dec.(u)) ();
    (* 自己辺は最初に緩和する必要がある *)
    Array.iteri (fun v c ->
      if 0 < Weight.compare Weight.zero c then
        inc.(v) <- (v, v, c) :: inc.(v)) self;
    d.(s) <- Weight.zero;
    (* 残りの反復回数 *)
    let i = ref n in
    (* 更新が行われたか *)
    let is_modified = ref true in
    (* 各辺の処理 *)
    let f (u, v, c) =
      let open Weight in
      (* 原点から到達できない頂点は更新しない *)
      if 0 < Weight.compare inf d.(u) then
        let dv = if 0 <= Weight.compare neg_inf d.(u) then neg_inf else d.(u) + c in
        if 0 < Weight.compare d.(v) dv then
          (is_modified := true; d.(v) <- if !i <= succ n lsr 1 then neg_inf else dv) in
    while 0 < !i && !is_modified do
      is_modified := false;
      (* インデックスが増加する辺の処理 *)
      for u = 0 to n - 1 do
        List.iter f inc.(u)
      done;
      (* インデックスが減少する辺の処理 *)
      for u = n - 1 downto 0 do
        List.iter f dec.(u)
      done;
      decr i
    done;
    Array.get d
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
