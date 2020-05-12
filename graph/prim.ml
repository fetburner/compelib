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
  type 'a church_list = { fold : 'b. ('a -> 'b -> 'b) -> 'b -> 'b }

  val prim :
    (* 隣接リスト *)
    (Vertex.t -> (Vertex.t * Weight.t) church_list) ->
    (* 始点 *)
    Vertex.t ->
    (* 最小全域木に含まれる辺のリスト
       畳み込む関数と初期値まで与えられて初めて計算を開始するので，
       最小全域木に対して色々な操作をしたい場合は，
       一度リストにして覚えておくとよい．
       最小全域木の重さの和だけ欲しいとか一回しか使わない場合は，
       直接畳み込む関数と初期値を与えるとdeforestationになっていい感じ． *)
    (Vertex.t * Vertex.t * Weight.t) church_list
end =
struct
  module VSet = Set.Make (Vertex)
  module WMap = Map.Make (Weight)

  type 'a church_list = { fold : 'b. ('a -> 'b -> 'b) -> 'b -> 'b }

  let prim es s =
    { fold = fun f init ->
        (*
         * プリム法のメインループ
         * acc : 最小全域木に使うのが確定した辺を入れるやつ
         * vs : 訪れた頂点の集合
         * q : 訪れた頂点から伸びる辺が重み順に入ったヒープ
         *)
        let rec prim acc vs q =
          match WMap.min_binding q with
          | exception Not_found -> acc
          | (w, []) -> prim acc vs (WMap.remove w q)
          | (w, (u, v) :: rest) ->
              begin if VSet.mem v vs
              (* vは既に訪れていた *)
              then prim acc vs
              (* vはまだ訪れていなかった *)
              else prim_aux (f (u, v, w) acc) vs v end (WMap.add w rest q)
        (* vを訪れ，vから伸びる辺をキューに追加してループを続行する *)
        and prim_aux acc vs v q =
          prim acc (VSet.add v vs) @@
          Fun.flip (es v).fold q @@ fun (u, w) q ->
            (* 現時点で既に訪れている頂点への辺は追加しない *)
            if VSet.mem u vs
            then q
            else Fun.flip (WMap.update w) q @@ fun vus -> Some ((v, u) :: Option.value ~default:[] vus) in
        prim_aux init VSet.empty s WMap.empty }
end

(* sample code *)

module G = WeightedGraph (String) (struct
  type t = int
  let compare = compare
end);;

(Fun.flip G.prim "A" @@ fun v ->
  { G.fold = fun f ->
    List.fold_right f @@
    match v with
    | "A" -> [("B", 7); ("D", 5)]
    | "B" -> [("A", 7); ("C", 8); ("D", 9); ("E", 7)]
    | "C" -> [("B", 8); ("E", 5)]
    | "D" -> [("A", 5); ("B", 9); ("E", 15); ("F", 6)]
    | "E" -> [("B", 7); ("C", 5); ("D", 15); ("F", 8); ("G", 9)]
    | "F" -> [("D", 6); ("E", 8); ("G", 11)]
    | "G" -> [("E", 9); ("F", 11)] }).G.fold List.cons []
