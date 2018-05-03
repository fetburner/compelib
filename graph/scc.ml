module type GRAPH = sig
  type vertex
  type vertex_set
  type edge_set = vertex -> vertex_set
  type graph = vertex_set * edge_set
  val sort : graph -> vertex list
  val scc : graph -> vertex list list
end

module Graph (VSet : Set.S) :
  GRAPH
  with type vertex = VSet.elt and type vertex_set = VSet.t =
struct
  type vertex = VSet.elt
  type vertex_set = VSet.t
  type edge_set = vertex -> vertex_set
  type graph = vertex_set * edge_set

  let rec visit es v (vs, l) =
    if VSet.mem v vs then
      let (vs', l') =
        VSet.fold (visit es) (es v) (VSet.remove v vs, l) in
      (vs', v :: l')
    else (vs, l)

  let sort (vs, es) = snd (VSet.fold (visit es) vs (vs, []))

  let scc (vs, es) =
    sort (vs, es)
    |> List.rev
    |> List.fold_left (fun (vs, l) v ->
        if VSet.mem v vs then 
          let (vs', cs) = visit es v (vs, []) in
          (vs', cs :: l)
        else (vs, l)) (vs, [])
    |> snd
end

module Int = struct
  type t = int
  let compare = compare
end

module IntSet = Set.Make (Int)
module IntG = Graph (IntSet)

IntG.scc (IntSet.of_list [ 1; 2; 3; 4; 5; 6; 7 ], function
  | 1 -> IntSet.singleton 2
  | 2 -> IntSet.singleton 3
  | 3 -> IntSet.of_list [ 1; 4 ]
  | 4 -> IntSet.singleton 5
  | 5 -> IntSet.of_list [ 4; 6; 7 ]
  | 6 -> IntSet.empty
  | 7 -> IntSet.empty)

IntG.sort (IntSet.of_list [1; 2; 3; 4; 5; 6; 7], function
  | 1 -> IntSet.of_list [4; 6]
  | 2 -> IntSet.of_list [5; 7]
  | 3 -> IntSet.of_list [4; 6]
  | 4 -> IntSet.empty
  | 5 -> IntSet.singleton 7
  | 6 -> IntSet.singleton 5
  | 7 -> IntSet.empty)
