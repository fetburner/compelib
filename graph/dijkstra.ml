module WeightedDirectedGraph
: sig
  (* 配列を用いたダイクストラ法の実装
     単純な速さでは一番だが，インターフェースが不便 *)
  module ByArray : sig
    module Make :
      functor (Weight : sig
        type t
        val zero : t
        val ( + ) : t -> t -> t
        val compare : t -> t -> int
      end) ->
      sig
        (* 頂点を[0, n)の自然数に限定したグラフに対してのダイクストラ法 *)
        val dijkstra : 
        (* 頂点数n *)
        int ->
        (* 隣接リスト *)
        (int -> (int * Weight.t) list) ->
        (* 始点 *)
        int ->
        (* 始点から辿り着けなければNoneを返す関数
           この関数を覚えておけば，呼び出しごとの途中までの計算結果がシェアされる *)
        (int -> Weight.t option)
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
        (* 始点から辿り着けなければNoneを返す関数
           この関数を覚えておけば，呼び出しごとの途中までの計算結果がシェアされる *)
        (Vertex.t -> Weight.t option)
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
        (* 始点から辿り着けなければNoneを返す関数
           この関数を覚えておけば，呼び出しごとの途中までの計算結果がシェアされる *)
        (Vertex.t -> Weight.t option)
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
      val find : t -> vertex -> W.t option
      val update : t -> vertex -> W.t -> unit
    end) =
  struct
    module WMap = Map.Make (W)

    let dijkstra d e s =
      VArray.update d s W.zero;
      let q = ref (WMap.singleton W.zero [s]) in
      let rec dijkstra_aux t =
        match VArray.find d t, WMap.min_binding_opt !q with
        (* もう既に全ての頂点までの距離が分かっている *)
        | ans, None -> ans
        (* 既に終点までの距離が分かっているので返す *)
        | Some x as ans, Some (w, _) when W.compare x w <= 0 -> ans
        (* 終点までの距離が分かっていないので，ダイクストラ法を続行 *)
        | _, Some (w, us) ->
            q := WMap.remove w !q;
            Fun.flip List.iter us (fun u ->
              if 0 <= W.compare (Option.get (VArray.find d u)) w then
                (* 未だ頂点uを訪れていない *)
                Fun.flip List.iter (e u) @@ fun (v, c) ->
                  let open W in
                  match VArray.find d v with
                  | Some d when W.compare d (w + c) <= 0 -> ()
                  | _ ->
                      VArray.update d v (w + c);
                      q := Fun.flip (WMap.update (w + c)) !q @@
                             fun vs -> Some (v :: Option.value ~default:[] vs));
            dijkstra_aux t in
      dijkstra_aux
  end

  module ByArray = struct
    module Make (W : Weight) = struct
      module C = Core (W) (struct
        type t = W.t option array
        type vertex = int
        let find = Array.get 
        let update d v w = d.(v) <- Some w
      end)

      let dijkstra n e s = C.dijkstra (Array.make n None) e s
    end
  end

  module ByHashtbl = struct
    module Make (V : Hashtbl.HashedType) (W : Weight) = struct
      module VHash = Hashtbl.Make (V)
      module C = Core (W) (struct
        type t = W.t VHash.t
        type vertex = V.t
        let find = VHash.find_opt
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
        let find d v = VMap.find_opt v !d
        let update d v w = d := VMap.add v w !d
      end)

      let dijkstra e s = C.dijkstra (ref VMap.empty) e s
    end
  end
end

(* sample code *)

module Float = struct
  type t = float
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
    let zero = (0., fun xs -> xs)
    let ( + ) (c, f) (d, g) = (c +. d, fun xs -> f (g xs))
    let compare (c, _) (d, _) = compare c d
  end)

let e' =
  Fun.flip Array.mapi e @@ fun u ->
    List.map @@ fun (v, c) ->
      let s = Printf.sprintf "%d->%d" u v in
      (v, (c, fun xs -> s :: xs));;

List.map (fun (Some (c, f)) -> (c, f [])) @@
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
