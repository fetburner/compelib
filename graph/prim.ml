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
  val prim :
    (* 隣接リスト *)
    (Vertex.t -> (Vertex.t * Weight.t) list) ->
    (* 始点 *)
    Vertex.t ->
    (* 最小全域木に含まれる辺のリスト *)
    (Vertex.t * Vertex.t * Weight.t) list
end =
struct
  module VSet = Set.Make (Vertex)
  module WMap = Map.Make (Weight)

  let prim es =
    (*
     * プリム法のメインループ
     * es : 隣接リスト
     * vs : 訪れた頂点の集合
     * q : 訪れた頂点から伸びる辺が重み順に入ったヒープ
     * acc : 最小全域木に使うのが確定した辺を入れるやつ
     *)
    let rec prim acc vs q =
      match WMap.min_binding_opt q with
      | None -> acc
      | Some (w, []) -> prim acc vs (WMap.remove w q)
      | Some (w, (u, v) :: rest) ->
          ( if VSet.mem v vs
            (* vは既に訪れていた *)
            then prim acc vs
            (* vはまだ訪れていなかった *)
            else prim_aux ((u, v, w) :: acc) vs v ) (WMap.add w rest q)
    (* vを訪れ，vから伸びる辺をキューに追加してループを続行する *)
    and prim_aux acc vs v q =
      prim acc (VSet.add v vs) @@
      List.fold_right (fun (u, w) ->
        WMap.update w @@ fun vus -> Some ((v, u) :: Option.value ~default:[] vus)) (es v) q in
    Fun.flip (prim_aux [] VSet.empty) WMap.empty
end

(* sample code *)

module G = WeightedGraph (String) (struct
  type t = int
  let compare = compare
end);;

G.prim (function
  | "A" -> [("B", 7); ("D", 5)]
  | "B" -> [("A", 7); ("C", 8); ("D", 9); ("E", 7)]
  | "C" -> [("B", 8); ("E", 5)]
  | "D" -> [("A", 5); ("B", 9); ("E", 15); ("F", 6)]
  | "E" -> [("B", 7); ("C", 5); ("D", 15); ("F", 8); ("G", 9)]
  | "F" -> [("D", 6); ("E", 8); ("G", 11)]
  | "G" -> [("E", 9); ("F", 11)]) "A";;
