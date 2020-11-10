(* sample code *)

module G = Compelib.BellmanFord.F (Int)
  (struct
    type t = int
    let zero = 0
    let inf = max_int
    let neg_inf = min_int
    let ( + ) = ( + )
    let compare = compare
  end)
  (struct
    type t = int array
    type key = int
    type elt = int
    type size = int
    let make = Fun.flip Array.make max_int
    let get = Array.get
    let set = Array.set
    let size_to_int = Fun.id
  end)

let%test _ =
  List.init 8 (G.bellman_ford 8
    (List.map (fun (u, v, c) -> (u, v, ( + ) c))
    [ (1, 2, 1); (2, 3, 1); (3, 7, 1); (4, 5, -1); (5, 6, -1); (6, 4, -1) ]) 1) =
  [ max_int; 0; 1; 2; max_int; max_int; max_int; 3]

let%test _ =
  List.init 8 (G.bellman_ford 8
    (List.map (fun (u, v, c) -> (u, v, ( + ) c))
    [ (1, 2, 1); (2, 3, 1); (3, 7, 1); (4, 5, -1); (5, 6, -1); (6, 4, -1); (7, 4, -1) ]) 1) =
  [max_int; 0; 1; 2; min_int; min_int; min_int; 3]

let%test _ =
  List.init 8 (G.bellman_ford 8
    (List.map (fun (u, v, c) -> (u, v, ( + ) c))
    [ (1, 2, 1); (2, 3, 1); (3, 7, 1); (4, 5, -1); (5, 6, -1); (6, 4, -1); (7, 4, -1); (4, 7, 1) ]) 1) =
  [max_int; 0; 1; 2; min_int; min_int; min_int; min_int]
