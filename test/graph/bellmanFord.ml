(* sample code *)

module V = struct
  type t = int
  type set = int
  let universe = 8
  let cardinal_universe = 8
  let compare = compare
end

module D = struct
  type t = int
  let zero = 0
  let inf = max_int
  let neg_inf = min_int
  let compare = compare
end

module G = Compelib.BellmanFord.F
  (struct
    type t = int array
    type key = int
    type elt = int
    type size = int
    let make = Fun.flip Array.make max_int
    let get = Array.get
    let set = Array.set
  end)

let%test _ =
  List.init 8 (G.shortest_path
    (module struct
      module Vertex = V
      module Distance = D
      module Edge = struct
        type t = int * int * int
        let source (u, _, _) = u
        let destination (_, v, _) = v
        let add_weight (_, _, c) = ( + ) c
        let universe = [ (1, 2, 1); (2, 3, 1); (3, 7, 1); (4, 5, -1); (5, 6, -1); (6, 4, -1) ]
      end
    end) 1)
  = [ max_int; 0; 1; 2; max_int; max_int; max_int; 3]

let%test _ =
  List.init 8 (G.shortest_path
    (module struct
      module Vertex = V
      module Distance = D
      module Edge = struct
        type t = int * int * int
        let source (u, _, _) = u
        let destination (_, v, _) = v
        let add_weight (_, _, c) = ( + ) c
        let universe = [ (1, 2, 1); (2, 3, 1); (3, 7, 1); (4, 5, -1); (5, 6, -1); (6, 4, -1); (7, 4, -1) ]
      end
    end) 1)
  = [max_int; 0; 1; 2; min_int; min_int; min_int; 3]

let%test _ =
  List.init 8 (G.shortest_path
    (module struct
      module Vertex = V
      module Distance = D
      module Edge = struct
        type t = int * int * int
        let source (u, _, _) = u
        let destination (_, v, _) = v
        let add_weight (_, _, c) = ( + ) c
        let universe = [ (1, 2, 1); (2, 3, 1); (3, 7, 1); (4, 5, -1); (5, 6, -1); (6, 4, -1); (7, 4, -1); (4, 7, 1) ]
      end
    end) 1)
  = [max_int; 0; 1; 2; min_int; min_int; min_int; min_int]
