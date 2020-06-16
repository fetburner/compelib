module WeightedDirectedGraph
  (* 辺の重み *)
  (Weight : sig
    type t
    val zero : t
    val compare : t -> t -> int
  end)
  (* 経路長を優先度としたヒープの実装 *)
  (Heap : sig
    type t
    type elt (* 頂点に相当 *)
    type key = Weight.t (* 経路長に相当 *)
    (* ヒープが空ならNoneを，
       そうでなければ経路長が最小となるbindingを一つ以上返す
       返したbindingは全てヒープから削除される *)
    val take_min_bindings : t -> (key * elt list) option
    (* ヒープにbindingを追加する
       既に同じ頂点についてのbindingが追加されていたら，
       経路長の短い方だけを残しても良いし，何も考えずに追加してもよい．
       前者の実装ならダイクストラ法の実装が時間計算量O((V + E) log V)，
       空間計算量O(V)に改善する *)
    val add : t -> key -> elt -> unit
  end)
  (* 頂点を添字，経路長を要素とした配列の実装 *)
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

  (* ダイクストラ法で最短経路を求める関数 *)
  val shortest_path :
    (* 空なヒープ *)
    Heap.t ->
    (* 全ての頂点についての経路長が無限大で初期化された配列 *)
    Array.t ->
    (* 最短経路を求めたいグラフの，ある頂点から伸びる辺に対してのイテレータ *)
    (vertex -> (vertex -> (weight -> weight) (* 辺を通った際のコストを加算する関数 *) -> unit) -> unit) ->
    (* 始点 *)
    vertex ->
    (* 終点を受け取って，始点からの最短距離を返す関数
       始点から辿り着けない場合，無限大を返す
       この関数を覚えておけば，呼び出しごとの途中までの計算結果がシェアされる *)
    (vertex -> weight)
end
= struct
  type weight = Array.elt
  type vertex = Array.key

  let shortest_path q d es s =
    (* 始点への経路長を0にする *)
    Array.set d s Weight.zero;
    (* 既に最短距離が確定した辺へのクエリを高速化するため，
       ヒープの最小要素をメモしておく *)
    let min_bindings = ref (Some (Weight.zero, [s])) in
    let rec dijkstra t =
      match !min_bindings with
      (* もう既に全ての頂点までの距離が分かっている *)
      | None -> Array.get d t
      | Some (w, us) ->
          match Array.get d t with
          (* 既に終点までの距離が分かっているので返す *)
          | x when 0 <= Weight.compare w x -> x
          (* 終点までの距離が分かっていないので，ダイクストラ法を続行 *)
          | _ ->
              List.iter (fun u ->
                (* 未だ頂点uを訪れていない *)
                if 0 <= Weight.compare (Array.get d u) w then
                  es u @@ fun v f ->
                    (* uからvに伸びる辺を通った際の経路長 *)
                    let c = f w in
                    if 0 < Weight.compare (Array.get d v) c then
                      (Heap.add q c v; Array.set d v c)) us;
              min_bindings := Heap.take_min_bindings q;
              dijkstra t in
    dijkstra
end

(* 使用例 *)

(* 最短距離だけ求める *)
module Float = struct
  type t = float
  let zero = 0.
  let compare = compare
end

module FloatMap = Map.Make (Float)

module G = WeightedDirectedGraph (Float)
  (struct
    type t = int list FloatMap.t ref
    type elt = int
    type key = float
    let take_min_bindings q =
      match FloatMap.min_binding !q with
      | exception Not_found -> None
      | (w, _) as p -> q := FloatMap.remove w !q; Some p
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

let e =
  [|[ (1, 7); (2, 9); (5, 14) ];
    [ (0, 7); (2, 10); (3, 15) ];
    [ (0, 9); (1, 10); (3, 11); (5, 2) ];
    [ (1, 15); (2, 11); (4, 6) ];
    [ (3, 6); (5, 9) ];
    [ (0, 14); (2, 2); (4, 9) ]|];;

Array.init 7 @@ Fun.flip (G.shortest_path (ref FloatMap.empty) (Array.make 7 infinity)) 0 @@ fun u f ->
  Fun.flip List.iter e.(u) @@ fun (v, c) -> f v @@ ( +. ) @@ float_of_int c;;

(* 経路長をMapに保存する *)
module IntMap = Map.Make (struct
  type t = int
  let compare = compare
end)

module G = WeightedDirectedGraph (Float)
  (struct
    type t = int list FloatMap.t ref
    type elt = int
    type key = float
    let take_min_bindings q =
      match FloatMap.min_binding !q with
      | exception Not_found -> None
      | (w, _) as p -> q := FloatMap.remove w !q; Some p
    let add q w v  =
      q := FloatMap.update w (fun vs -> Some (v :: Option.value ~default:[] vs)) !q
  end)
  (struct
    type t = float IntMap.t ref
    type key = int
    type elt = float
    let get m k = try IntMap.find k !m with Not_found -> infinity
    let set m k x = m := IntMap.add k x !m
  end);;

Array.init 10 @@
  Fun.flip (G.shortest_path (ref FloatMap.empty) (ref IntMap.empty)) 0 @@ fun u f ->
    Fun.flip List.iter e.(u) @@ fun (v, c) -> f v @@ ( +. ) @@ float_of_int c;;

(* 無限グラフも可 *)
Array.init 10 @@
Fun.flip (G.shortest_path (ref FloatMap.empty) (ref IntMap.empty)) 0 @@ fun u f -> f (u + 1) @@ ( +. ) 1.;;

(* 経路復元する *)

module WeightedRoute = struct
  type t = float * int list
  let zero = (0., [0])
  let compare (c, r) (c', r') = 
    match compare c c' with
    | 0 -> compare (List.hd r) (List.hd r')
    | n -> n
end

module WeightedRouteMap = Map.Make (WeightedRoute)

module G = WeightedDirectedGraph (WeightedRoute)
  (struct
    type t = int list WeightedRouteMap.t ref
    type elt = int
    type key = WeightedRoute.t
    let take_min_bindings q =
      match WeightedRouteMap.min_binding !q with
      | exception Not_found -> None
      | (w, _) as p -> q := WeightedRouteMap.remove w !q; Some p
    let add q w v  =
      q := WeightedRouteMap.update w (fun vs -> Some (v :: Option.value ~default:[] vs)) !q
  end)
  (struct
    type t = WeightedRoute.t array
    type key = int
    type elt = WeightedRoute.t
    let get = Array.get
    let set = Array.set
  end);;

Array.init 7 @@ Fun.flip (G.shortest_path (ref WeightedRouteMap.empty) (Array.make 7 (infinity, []))) 0 @@ fun u f ->
  Fun.flip List.iter e.(u) @@ fun (v, c) -> f v @@ fun (w, r) -> (float_of_int c +. w, v :: r)
