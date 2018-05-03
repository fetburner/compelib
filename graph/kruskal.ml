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
  val kruskal :
    (* 頂点の数 *)
    int ->
    (* 辺のリスト *)
    (Vertex.t * Vertex.t * Weight.t) list ->
    (* 最小全域木に含まれる辺のリスト *)
    (Vertex.t * Vertex.t * Weight.t) list
end =
struct
  module UF = PUnionFind (Vertex)

  let kruskal n es =
    List.sort (fun (_, _, w) (_, _, w') -> Weight.compare w w') es
    |> List.fold_left (fun (uf, acc) (u, v, w) ->
        if UF.compare_class (UF.find uf u) (UF.find uf v) = 0
        then (uf, acc)
        else (UF.union uf u v, (u, v, w) :: acc)) (UF.make n, [])
    |> snd
end

(* sample code *)

module G = WeightedGraph (String) (struct
  type t = int
  let compare = compare
end);;

G.kruskal 7
  [("A", "B", 7); ("A", "D", 5);
   ("B", "A", 7); ("B", "C", 8); ("B", "D", 9); ("B", "E", 7);
   ("C", "B", 8); ("C", "E", 5);
   ("D", "A", 5); ("D", "B", 9); ("D", "E", 15); ("D", "F", 6);
   ("E", "B", 7); ("E", "C", 5); ("E", "D", 15); ("E", "F", 8); ("E", "G", 9);
   ("F", "D", 6); ("F", "E", 8); ("F", "G", 11);
   ("G", "E", 9); ("G", "F", 11)];;
