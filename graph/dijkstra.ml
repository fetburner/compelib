module WeightedDirectedGraph
: sig
  (* 配列を用いたダイクストラ法の実装
     単純な速さでは一番だが，インターフェースが不便 *)
  module ByArray : sig
    module Make :
      functor (Weight : sig
        type t
        val inf : t
        val zero : t
        val ( + ) : t -> t -> t
        val compare : t -> t -> int
      end) ->
      sig
        (* 頂点を[0, n)の自然数に限定したグラフに対してのダイクストラ法
           時間計算量O(E log E)なので，疎なグラフなら速い *)
        val dijkstra :
        (* 頂点数n *)
        int ->
        (* 隣接リスト *)
        (int -> (int * Weight.t) list) ->
        (* 始点 *)
        int ->
        (* 始点から辿り着けなければinfを返す関数
           この関数を覚えておけば，呼び出しごとの途中までの計算結果がシェアされる *)
        (int -> Weight.t)

        (* 頂点を[0, n)の自然数に限定したグラフに対してのダイクストラ法
           時間計算量O(V^2)なので，密なグラフなら速い *)
        val dijkstra_dense :
        (* 頂点数n *)
        int ->
        (* 隣接リスト *)
        (int -> (int * Weight.t) list) ->
        (* 始点 *)
        int ->
        (* 始点から辿り着けなければinfを返す関数
           この関数を覚えておけば，呼び出しごとの途中までの計算結果がシェアされる *)
        (int -> Weight.t)
      end
  end

  (* ハッシュテーブルを用いたダイクストラ法の実装
     配列を用いた実装よりは扱いやすいインターフェースを持つ
     ハッシュ関数を上手く選べば配列を用いた実装より1.5倍遅い程度ですむ *)
  module ByHashtbl : sig
    module Make :
      functor
      (* 頂点 *)
      (Vertex : Hashtbl.HashedType)
      (* 辺の重み *)
      (Weight : sig
        type t
        val zero : t
        val ( + ) : t -> t -> t
        val compare : t -> t -> int
      end) ->
      sig
        val dijkstra :
        (* 頂点数（Hashtbl.tを用いるので目安程度） *)
        int ->
        (* 隣接リスト *)
        (Vertex.t -> (Vertex.t * Weight.t) list) ->
        (* 始点 *)
        Vertex.t ->
        (* 始点から辿り着けなければNot_foundを投げる関数
           この関数を覚えておけば，呼び出しごとの途中までの計算結果がシェアされる *)
        (Vertex.t -> Weight.t)
      end
  end

  (* Mapを用いたダイクストラ法の実装
     配列を用いた実装より4倍ぐらい遅いが，
     一番扱いやすいインターフェースを持ち，無限グラフにも対応可能 *)
  module ByMap : sig
    module Make :
      functor
      (* 頂点 *)
      (Vertex : Map.OrderedType)
      (* 辺の重み *)
      (Weight : sig
        type t
        val zero : t
        val ( + ) : t -> t -> t
        val compare : t -> t -> int
      end) ->
      sig
        val dijkstra :
        (* 隣接リスト *)
        (Vertex.t -> (Vertex.t * Weight.t) list) ->
        (* 始点 *)
        Vertex.t ->
        (* 始点から辿り着けなければNot_foundを投げる関数
           この関数を覚えておけば，呼び出しごとの途中までの計算結果がシェアされる *)
        (Vertex.t -> Weight.t)
      end
  end
end
= struct
  module type Weight = sig
    type t
    val zero : t
    val ( + ) : t -> t -> t
    val compare : t -> t -> int
  end

  (* 最短距離を格納するデータ構造を抽象化したダイクストラ法の実装 *)
  module Core
    (W : Weight)
    (* グラフの頂点を添字とした配列 *)
    (VArray : sig
      type t
      type vertex (* グラフの頂点 *)
      val find : t -> vertex -> W.t (* 最短距離が格納されていなければNot_foundを投げる *)
      val update : t -> vertex -> W.t -> unit
    end) =
  struct
    module WMap = Map.Make (W)

    let dijkstra d e s =
      VArray.update d s W.zero;
      let q = ref (WMap.singleton W.zero [s]) in
      let rec dijkstra_aux t =
        match WMap.min_binding !q with
        (* もう既に全ての頂点までの距離が分かっている *)
        | exception Not_found -> VArray.find d t
        | (w, us) ->
            match VArray.find d t with
            (* 既に終点までの距離が分かっているので返す *)
            | x when W.compare x w <= 0 -> x
            (* 終点までの距離が分かっていないので，ダイクストラ法を続行 *)
            | _ | exception Not_found ->
                q := WMap.remove w !q;
                List.iter (fun u ->
                  if 0 <= W.compare (VArray.find d u) w then
                    (* 未だ頂点uを訪れていない *)
                    List.iter (fun (v, c) ->
                      let open W in
                      match VArray.find d v with
                      | d when W.compare d (w + c) <= 0 -> ()
                      | _ | exception Not_found ->
                          VArray.update d v (w + c);
                          q := WMap.update (w + c) (fun vs -> Some (v :: Option.value ~default:[] vs)) !q) (e u)) us;
                dijkstra_aux t in
      dijkstra_aux
  end

  module ByArray = struct
    module Make (W : sig include Weight val inf : t end) = struct
      module C = Core (W) (struct
        type t = W.t array
        type vertex = int
        let find = Array.get
        let update = Array.set
      end)

      let dijkstra n e s = C.dijkstra (Array.make n W.inf) e s

      let dijkstra_dense n e s =
        let d = Array.make n W.inf in
        d.(s) <- W.zero;
        let rec dijkstra_dense_aux = function
          | [] -> ()
          | v :: vs ->
              let u, us = List.fold_left (fun (u, us) v ->
                if W.compare d.(u) d.(v) < 0
                then (u, v :: us)
                else (v, u :: us)) (v, []) vs in
              List.iter (fun (v, c) ->
                let open W in
                if 0 < W.compare d.(v) (d.(u) + c) then
                  d.(v) <- d.(u) + c) (e u);
              dijkstra_dense_aux us in
        dijkstra_dense_aux (List.init n Fun.id);
        Array.get d
      end
  end

  module ByHashtbl = struct
    module Make (V : Hashtbl.HashedType) (W : Weight) = struct
      module VHash = Hashtbl.Make (V)
      module C = Core (W) (struct
        type t = W.t VHash.t
        type vertex = V.t
        let find = VHash.find
        let update = VHash.replace
      end)

      let dijkstra n e s = C.dijkstra (VHash.create n) e s
    end
  end

  module ByMap = struct
    module Make (V : Map.OrderedType) (W : Weight) = struct
      module VMap = Map.Make (V)
      module C = Core (W) (struct
        type t = W.t VMap.t ref
        type vertex = V.t
        let find d v = VMap.find v !d
        let update d v w = d := VMap.add v w !d
      end)

      let dijkstra e s = C.dijkstra (ref VMap.empty) e s
    end
  end
end

(* sample code *)

module Float = struct
  type t = float
  let inf = infinity
  let zero = 0.
  let ( + ) = ( +. )
  let compare = compare
end

module G = WeightedDirectedGraph.ByArray.Make (Float)

let e =
  [|[ (1, 7.); (2, 9.); (5, 14.) ];
    [ (0, 7.); (2, 10.); (3, 15.) ];
    [ (0, 9.); (1, 10.); (3, 11.); (5, 2.) ];
    [ (1, 15.); (2, 11.); (4, 6.) ];
    [ (3, 6.); (5, 9.) ];
    [ (0, 14.); (2, 2.); (4, 9.) ]|];;

List.init 7 @@ Fun.flip (G.dijkstra 7) 0 @@ Array.get e

module G' = WeightedDirectedGraph.ByArray.Make
  (struct
    type t = float * (string list -> string list)
    let inf = (infinity, fun xs -> xs)
    let zero = (0., fun xs -> xs)
    let ( + ) (c, f) (d, g) = (c +. d, fun xs -> f (g xs))
    let compare (c, _) (d, _) = compare c d
  end)

let e' =
  Fun.flip Array.mapi e @@ fun u ->
    List.map @@ fun (v, c) ->
      let s = Printf.sprintf "%d->%d" u v in
      (v, (c, fun xs -> s :: xs));;

List.map (fun (c, f) -> (c, f [])) @@
List.init 6 @@ Fun.flip (G'.dijkstra 7) 0 @@ Array.get e';;

(* 無限グラフ!!! *)
module IntPair = struct
  type t = int * int
  let compare = compare
end

module G = WeightedDirectedGraph.ByMap.Make (IntPair) (Float)

let d = Fun.flip G.dijkstra (0, 0) @@ fun (x, y) ->
  List.map (fun v -> (v, float_of_int (abs x + abs y)))
  [(x + 1, y); (x - 1, y); (x, y + 1); (x, y - 1)];;
List.init 10 @@ fun x ->
  List.init 10 @@ fun y -> d (x, y);;
