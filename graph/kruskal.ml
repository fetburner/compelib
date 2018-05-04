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
  (* 配列版の実装 *)
  val raw_kruskal :
    (* 頂点の数n *)
    int ->
    (* 辺のリスト *)
    (* 頂点は0からn-1までの整数でなくてはならない *)
    (int * int * Weight.t) list ->
    (* 最小全域木に含まれる辺のリスト *)
    (int * int * Weight.t) list

  (* 座標圧縮により，頂点に様々な型を使えるようにしたバージョン *)
  val kruskal :
    (* 辺のリスト *)
    (Vertex.t * Vertex.t * Weight.t) list ->
    (* 最小全域木に含まれる辺のリスト *)
    (Vertex.t * Vertex.t * Weight.t) list
end =
struct
  module CC = CoordComp (Vertex)

  let raw_kruskal n es =
    let uf = RawUnionFind.make n in
    List.fold_left (fun acc (u, v, w) ->
      if RawUnionFind.compare_class (RawUnionFind.find uf u) (RawUnionFind.find uf v) = 0
      then acc
      else begin
        RawUnionFind.unite uf u v;
        (u, v, w) :: acc
      end) [] (List.sort (fun (_, _, w) (_, _, w') -> Weight.compare w w') es)

  let kruskal es =
    let (n, comp, decomp) =
      CC.compress @@
        List.concat @@
          List.map (fun (u, v, _) -> [u; v]) es in
    List.map (fun (u, v, c) -> (comp u, comp v, c)) es
    |> raw_kruskal n
    |> List.map (fun (u, v, c) -> (decomp u, decomp v, c))
end

(* sample code *)

module G = WeightedGraph (String) (struct
  type t = int
  let compare = compare
end);;

G.kruskal
  [("A", "B", 7); ("A", "D", 5);
   ("B", "A", 7); ("B", "C", 8); ("B", "D", 9); ("B", "E", 7);
   ("C", "B", 8); ("C", "E", 5);
   ("D", "A", 5); ("D", "B", 9); ("D", "E", 15); ("D", "F", 6);
   ("E", "B", 7); ("E", "C", 5); ("E", "D", 15); ("E", "F", 8); ("E", "G", 9);
   ("F", "D", 6); ("F", "E", 8); ("F", "G", 11);
   ("G", "E", 9); ("G", "F", 11)];;
