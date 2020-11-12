(* sample code *)

module IntG = Compelib.Scc.F
  (struct
    type t = int
    type vertex = int
    let rec fold f n acc =
      if n = 0
      then acc
      else fold f (n - 1) (f n acc)
  end)
  (struct
    type t = bool array
    type key = int
    type elt = bool
    type size = int
    let make n = Array.make (n + 1) false
    let get = Array.get
    let set = Array.set
  end)

let%test _ =
  List.map (List.sort_uniq compare)
    [[2; 3; 1]; [4; 5]; [6]; [7]] =
  List.map (List.sort_uniq compare)
    (IntG.scc 7 (function
      | 1 -> [2]
      | 2 -> [3]
      | 3 -> [1; 4]
      | 4 -> [5]
      | 5 -> [4; 6; 7]
      | 6 -> []
      | 7 -> []))

let%test _ =
  IntG.sort 7 (function
    | 1 -> [2]
    | 2 -> [3]
    | 3 -> [1; 4]
    | 4 -> [5]
    | 5 -> [4; 6; 7]
    | 6 -> []
    | 7 -> []) =
  [3; 1; 2; 5; 4; 6; 7]
