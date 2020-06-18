module WeightedGraph
  (* 辺の重み *)
  (Weight : sig
    type t
    val zero : t
    val ( + ) : t -> t -> t
    val compare : t -> t -> int
  end)
  (* 辺の重みを優先度としたヒープの実装 *)
  (Heap : sig
    type t
    type elt (* 頂点に相当 *)
    type key = Weight.t (* 辺の重みに相当 *)
    (* ヒープが空ならNoneを，
       そうでなければ重みが最小となるbindingを一つ返す
       返したbindingはヒープから削除される *)
    val take_min_binding : t -> (key * elt) option
    (* ヒープにbindingを追加する
       既に同じ頂点についてのbindingが追加されていたら，
       重みが小さい方だけを残しても良いし，何も考えずに追加してもよい．
       前者の実装ならプリム法の実装が時間計算量O((V + E) log V)，
       空間計算量O(V)に改善する *)
    val add : t -> key -> elt -> unit
  end)
  (* 頂点を添字，辺の重みを要素とした配列の実装 *)
  (Array : sig
    type t
    type key = Heap.elt
    type elt = Heap.key
    val get : t -> key -> elt
    val set : t -> key -> elt -> unit
  end)
: sig
  type weight = Array.elt
  type vertex = Array.key

  (* プリム法で最小全域木を求める *)
  val minimum_spanning_tree :
    (* 空なヒープ *)
    Heap.t ->
    (* 全ての頂点について無限大で初期化された配列 *)
    Array.t ->
    (* 最小全域木を求めたいグラフの，ある頂点から伸びる辺に対してのイテレータ *)
    (vertex -> (vertex -> weight -> unit) -> unit) ->
    (* 頂点の一つ（これは必ず最小全域木に含まれる） *)
    vertex ->
    (* 最小全域木に含まれる辺の重みの総和 *)
    weight
end
= struct
  type weight = Array.elt
  type vertex = Array.key

  let minimum_spanning_tree q d es s =
    let rec prim acc u =
      Array.set d u Weight.zero;
      es u (fun v w ->
        if 0 < Weight.compare (Array.get d v) w then
          (Array.set d v w; Heap.add q w v));
      prim' acc
    and prim' acc =
      match Heap.take_min_binding q with
      | None -> acc
      | Some (w, u) ->
          if 0 < Weight.compare w (Array.get d u)
          then prim' acc
          else prim (Weight.( + ) acc w) u in
    prim Weight.zero s
end

(* 使用例 *)

(* 重みの総和だけ分かれば良い場合 *)

module Float = struct
  type t = float
  let zero = 0.
  let ( + ) = ( +. )
  let compare = compare
end

module FloatMap = Map.Make (Float)

module G = WeightedGraph (Float)
  (struct
    type t = int list FloatMap.t ref
    type key = float
    type elt = int
    let take_min_binding q =
      match FloatMap.min_binding !q with
      | exception Not_found -> None
      | (w, [v]) -> q := FloatMap.remove w !q; Some (w, v)
      | (w, v :: vs) -> q := FloatMap.add w vs !q; Some (w, v)
    let add q w v  =
      q := FloatMap.update w (fun vs -> Some (v :: Option.value ~default:[] vs)) !q
  end)
  (struct
    type t = float array
    type key = int
    type elt = float
    let get = Array.get
    let set = Array.set
  end)

let es =
  [|[(1, 7.); (3, 5.)];
    [(0, 7.); (2, 8.); (3, 9.); (4, 7.)];
    [(1, 8.); (4, 5.)];
    [(0, 5.); (1, 9.); (4, 15.); (5, 6.)];
    [(1, 7.); (2, 5.); (3, 15.); (5, 8.); (6, 9.)];
    [(3, 6.); (4, 8.); (6, 11.)];
    [(4, 9.); (5, 11.)]|];;

G.minimum_spanning_tree (ref FloatMap.empty) (Array.make 7 infinity)
  (fun u f -> List.iter (fun (v, w) -> f v w) es.(u)) 0;;

(* 使った辺を列挙したい場合 *)

module WeightedRoute = struct
  (* 重みは同じでも違う頂点への辺を同一視されないようにする *)
  type t = float * int * ((int * int * float) list -> (int * int * float) list)
  let zero = (0., min_int, fun xs -> xs)
  let ( + ) (w, u, f) (w', v, g) = (w +. w', v, fun xs -> g (f xs))
  let compare (w, u, _) (w', v, _) =
    match compare w w' with
    | 0 -> compare u v
    | x -> x
end

module WeightedRouteMap = Map.Make (WeightedRoute)

module G = WeightedGraph (WeightedRoute)
  (struct
    type t = int list WeightedRouteMap.t ref
    type key = WeightedRoute.t
    type elt = int
    let take_min_binding q =
      match WeightedRouteMap.min_binding !q with
      | exception Not_found -> None
      | (w, [v]) -> q := WeightedRouteMap.remove w !q; Some (w, v)
      | (w, v :: vs) -> q := WeightedRouteMap.add w vs !q; Some (w, v)
    let add q w v  =
      q := WeightedRouteMap.update w (fun vs -> Some (v :: Option.value ~default:[] vs)) !q
  end)
  (struct
    type t = WeightedRoute.t array
    type key = int
    type elt = WeightedRoute.t
    let get = Array.get
    let set = Array.set
  end)

let (w, _, f) =
  G.minimum_spanning_tree (ref WeightedRouteMap.empty)
    (Array.make 7 (infinity, max_int, fun xs -> xs))
    (fun u f -> List.iter (fun (v, w) -> f v (w, v, fun xs -> (u, v, w) :: xs)) es.(u)) 0;;
w, f [];;
