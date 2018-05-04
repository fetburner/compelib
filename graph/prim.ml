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

  (*
   * プリム法のメインループ
   * es : 隣接リスト
   * vs : 訪れた頂点の集合
   * q : 訪れた頂点から伸びる辺が重み順に入ったヒープ
   * acc : 最小全域木に使うのが確定した辺を入れるやつ
   *)
  let rec prim_aux es acc vs q =
    match WMap.min_binding q with
    | exception Not_found -> acc
    | (w, []) -> prim_aux es acc vs (WMap.remove w q)
    | (w, (u, v) :: rest) ->
        if VSet.mem v vs then
          (* vは既に訪れていた *)
          prim_aux es acc vs (WMap.add w rest q)
        else
          (* vはまだ訪れていなかった *)
          prim_aux es ((u, v, w) :: acc) (VSet.add v vs) @@
            (* vから伸びる辺をキューに追加 *)
            List.fold_left (fun q (u, w) ->
              (* 現時点で既に訪れている頂点への辺は追加しない *)
              if VSet.mem u vs then q
              else WMap.add w ((v, u) :: try WMap.find w q with Not_found -> []) q) (WMap.add w rest q) (es v)

  let prim es s =
    prim_aux es [] (VSet.singleton s) @@
      (* 始点から伸びる辺をキューに入れておく *)
      List.fold_left (fun q (v, w) ->
        WMap.add w ((s, v) :: try WMap.find w q with Not_found -> []) q) WMap.empty (es s)
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
