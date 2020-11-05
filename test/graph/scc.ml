(* sample code *)

module IntSet = Set.Make (Int)
module IntG = Compelib.Scc.DirectedGraph (IntSet);;

let%test _ =
  List.map (List.sort_uniq compare)
    [[2; 3; 1]; [4; 5]; [6]; [7]] =
  List.map (List.sort_uniq compare)
    (IntG.scc [1; 2; 3; 4; 5; 6; 7] (function
      | 1 -> [2]
      | 2 -> [3]
      | 3 -> [1; 4]
      | 4 -> [5]
      | 5 -> [4; 6; 7]
      | 6 -> []
      | 7 -> []))

let%test _ =
  IntG.sort [1; 2; 3; 4; 5; 6; 7] (function
    | 1 -> [2]
    | 2 -> [3]
    | 3 -> [1; 4]
    | 4 -> [5]
    | 5 -> [4; 6; 7]
    | 6 -> []
    | 7 -> []) =
  [3; 1; 2; 5; 4; 6; 7]
