module WeightedGraph
  (Vertex : sig
    type t
    val compare : t -> t -> int
  end)
  (Weight : sig
    type t
    val compare : t -> t -> int
  end) :
sig
  (*
   * HashtblやMapを用いているとプリム法と大差ない計算時間になってしまうため，
   * より汎用的な実装が欲しければそちらを当たること
   *)
  val kruskal :
    (* 頂点の数n *)
    int ->
    (* 辺のリスト *)
    (* 頂点は0からn-1までの整数でなくてはならない *)
    (int * int * Weight.t) list ->
    (* 最小全域木に含まれる辺のリスト *)
    (int * int * Weight.t) list
end =
struct
  let kruskal n es =
    let uf = RawUnionFind.make n in
    List.fold_left (fun acc (u, v, w) ->
      if RawUnionFind.compare_class (RawUnionFind.find uf u) (RawUnionFind.find uf v) = 0
      then acc
      else begin
        RawUnionFind.unite uf u v;
        (u, v, w) :: acc
      end) [] (List.sort (fun (_, _, w) (_, _, w') -> Weight.compare w w') es)
end

(* sample code *)

module G = WeightedGraph (String) (struct
  type t = int
  let compare = compare
end);;

G.kruskal 7
  [(0, 1, 7); (0, 3, 5);
   (1, 0, 7); (1, 2, 8); (1, 3, 9); (1, 4, 7);
   (2, 1, 8); (2, 4, 5);
   (3, 0, 5); (3, 1, 9); (3, 4, 15); (3, 5, 6);
   (4, 1, 7); (4, 2, 5); (4, 3, 15); (4, 5, 8); (4, 6, 9);
   (5, 3, 6); (5, 4, 8); (5, 6, 11);
   (6, 4, 9); (6, 5, 11)];;
