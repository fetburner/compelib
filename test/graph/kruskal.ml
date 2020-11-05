module G = Compelib.Kruskal.WeightedGraph (struct
  type t = int
  let compare = compare
end)

let%test _ =
  List.sort compare ((G.kruskal 7
    [(0, 1, 7); (0, 3, 5);
     (1, 0, 7); (1, 2, 8); (1, 3, 9); (1, 4, 7);
     (2, 1, 8); (2, 4, 5);
     (3, 0, 5); (3, 1, 9); (3, 4, 15); (3, 5, 6);
     (4, 1, 7); (4, 2, 5); (4, 3, 15); (4, 5, 8); (4, 6, 9);
     (5, 3, 6); (5, 4, 8); (5, 6, 11); (6, 4, 9); (6, 5, 11)]).fold List.cons []) =
  List.sort compare [(4, 6, 9); (1, 4, 7); (0, 1, 7); (3, 5, 6); (2, 4, 5); (0, 3, 5)]
