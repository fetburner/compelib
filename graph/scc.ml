module DirectedGraph
  (Vertex : sig
    type t
    val compare : t -> t -> int
  end) :
sig
  (* トポロジカルソート *)
  val sort :
    (* 頂点のリスト *)
    Vertex.t list ->
    (* 隣接リスト *)
    (Vertex.t -> Vertex.t list) ->
    Vertex.t list

  (* 強連結成分分解 *)
  val scc :
    (* 頂点のリスト *)
    Vertex.t list ->
    (* 隣接リスト *)
    (Vertex.t -> Vertex.t list) ->
    Vertex.t list list
end =
struct
  module VSet = Set.Make (Vertex)

  let rec visit es v (vs, l) =
    if VSet.mem v vs then
      let (vs', l') =
        List.fold_right (visit es) (es v) (VSet.remove v vs, l) in
      (vs', v :: l')
    else (vs, l)

  let sort vs es =
    snd (List.fold_right (visit es) vs (VSet.of_list vs, []))

  let scc vs es =
    sort vs es
    |> List.rev
    |> List.fold_left (fun (vs, l) v ->
        if VSet.mem v vs then 
          let (vs', cs) = visit es v (vs, []) in
          (vs', cs :: l)
        else (vs, l)) (VSet.of_list vs, [])
    |> snd
end

(* sample code *)

module Int = struct
  type t = int
  let compare = compare
end

module IntSet = Set.Make (Int)
module IntG = DirectedGraph (Int);;

IntG.scc [1; 2; 3; 4; 5; 6; 7] (function
  | 1 -> [2]
  | 2 -> [3]
  | 3 -> [1; 4]
  | 4 -> [5]
  | 5 -> [4; 6; 7]
  | 6 -> []
  | 7 -> []);;

IntG.sort [1; 2; 3; 4; 5; 6; 7] (function
  | 1 -> [2]
  | 2 -> [3]
  | 3 -> [1; 4]
  | 4 -> [5]
  | 5 -> [4; 6; 7]
  | 6 -> []
  | 7 -> [])
