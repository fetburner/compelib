module WeightedDirectedGraph
: sig
  (* 配列を用いたダイクストラ法の実装
     単純な速さでは一番だが，インターフェースが不便 *)
  module ByArray : sig
    module Make :
      functor
      (* 辺の重さ *)
      (Weight : sig
        type t
        val inf : t
        val zero : t
        val ( + ) : t -> t -> t
        val compare : t -> t -> int
      end)
      (* 道の集合の表現 *)
      (Path : sig
        type t
        (* 辺の名前 *)
        type edge
        (* 長さ0の道だけのsingleton *)
        val nil : t
        (* 空集合 *)
        val empty : t
        (* 与えられた集合に属する道について，それぞれ末尾に辺を付け足した集合 *)
        val snoc : t -> edge -> t
        (* 道の集合のunion
           最短経路は一つだけ分かればよい場合はFun.constを与えるとよい *)
        val union : t -> t -> t
      end) ->
        sig
          type 'a church_list = { fold : 'b. ('a -> 'b -> 'b) -> 'b -> 'b }

          (* 頂点を[0, n)の自然数に限定したグラフに対してのダイクストラ法
             時間計算量O(E log E)なので，疎なグラフなら速い *)
          val dijkstra :
          (* 頂点数n *)
          int ->
          (* 隣接リスト *)
          (int -> (int * Weight.t * Path.edge) church_list) ->
          (* 始点 *)
          int ->
          (* 始点からの最短距離を返す関数と，始点からの最短経路を返す関数の組
             始点から辿り着けない場合，前者はinfを，後者はPath.emptyを返す
             これらの関数を覚えておけば，呼び出しごとの途中までの計算結果がシェアされる *)
          (int -> Weight.t) * (int -> Path.t)

          (* 頂点を[0, n)の自然数に限定したグラフに対してのダイクストラ法
             時間計算量O(V^2)なので，密なグラフなら速い *)
          val dijkstra_dense :
          (* 頂点数n *)
          int ->
          (* 隣接リスト *)
          (int -> (int * Weight.t * Path.edge) church_list) ->
          (* 始点 *)
          int ->
          (* 始点からの最短距離を返す関数と，始点からの最短経路を返す関数の組
             始点から辿り着けない場合，前者はinfを，後者はPath.emptyを返す
             これらの関数を覚えておけば，呼び出しごとの途中までの計算結果がシェアされる *)
          (int -> Weight.t) * (int -> Path.t)
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
      (* 辺の重さ *)
      (Weight : sig
        type t
        val inf : t
        val zero : t
        val ( + ) : t -> t -> t
        val compare : t -> t -> int
      end)
      (* 道の集合の表現 *)
      (Path : sig
        type t
        (* 辺の名前 *)
        type edge
        (* 長さ0の道だけのsingleton *)
        val nil : t
        (* 与えられた集合に属する道について，それぞれ末尾に辺を付け足した集合 *)
        val snoc : t -> edge -> t
        (* 道の集合のunion
           最短経路は一つだけ分かればよい場合はFun.constを与えるとよい *)
        val union : t -> t -> t
      end) ->
      sig
        type 'a church_list = { fold : 'b. ('a -> 'b -> 'b) -> 'b -> 'b }

        val dijkstra :
        (* 頂点数（Hashtbl.tを用いるので目安程度） *)
        int ->
        (* 隣接リスト *)
        (Vertex.t -> (Vertex.t * Weight.t * Path.edge) church_list) ->
        (* 始点 *)
        Vertex.t ->
        (* 始点からの最短距離を返す関数と，始点からの最短経路を返す関数の組
           両者とも始点から辿り着けなければNot_foundを投げる
           これらの関数を覚えておけば，呼び出しごとの途中までの計算結果がシェアされる *)
        (Vertex.t -> Weight.t) * (Vertex.t -> Path.t)
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
      end)
      (* 道の集合の表現 *)
      (Path : sig
        type t
        (* 辺の名前 *)
        type edge
        (* 長さ0の道だけのsingleton *)
        val nil : t
        (* 与えられた集合に属する道について，それぞれ末尾に辺を付け足した集合 *)
        val snoc : t -> edge -> t
        (* 道の集合のunion
           最短経路は一つだけ分かればよい場合はFun.constを与えるとよい *)
        val union : t -> t -> t
      end) ->
      sig
        type 'a church_list = { fold : 'b. ('a -> 'b -> 'b) -> 'b -> 'b }

        val dijkstra :
        (* 隣接リスト *)
        (Vertex.t -> (Vertex.t * Weight.t * Path.edge) church_list) ->
        (* 始点 *)
        Vertex.t ->
        (* 始点からの最短距離を返す関数と，始点からの最短経路を返す関数の組
           両者とも始点から辿り着けなければNot_foundを投げる
           これらの関数を覚えておけば，呼び出しごとの途中までの計算結果がシェアされる *)
        (Vertex.t -> Weight.t) * (Vertex.t -> Path.t)
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

  module type Path = sig
    type t
    type edge
    val nil : t
    val snoc : t -> edge -> t
    val union : t -> t -> t
  end

  (* 最短距離を格納するデータ構造を抽象化したダイクストラ法の実装 *)
  module Core
    (W : Weight)
    (P : Path)
    (* グラフの頂点を添字とした配列 *)
    (VArray : sig
      type t
      type vertex (* グラフの頂点 *)
      val find : t -> vertex -> W.t * P.t (* 最短距離が格納されていなければNot_foundを投げる *)
      val update : t -> vertex -> W.t * P.t -> unit
    end) =
  struct
    type 'a church_list = { fold : 'b. ('a -> 'b -> 'b) -> 'b -> 'b }

    module WMap = Map.Make (W)

    let dijkstra d es s =
      VArray.update d s (W.zero, P.nil);
      let q = ref (WMap.singleton W.zero [s]) in
      (* 既に最短距離が確定した辺へのクエリを高速化するため，
         ヒープの最小要素をメモしておく *)
      let min_binding_opt = ref (Some (W.zero, [s])) in
      let rec dijkstra_aux t =
        match !min_binding_opt with
        (* もう既に全ての頂点までの距離が分かっている *)
        | None -> VArray.find d t
        | Some (w, us) ->
            match VArray.find d t with
            (* 既に終点までの距離と，全ての最短経路が分かっているので返す *)
            | (x, _) as ans when W.compare x w < 0 -> ans
            (* 終点までの距離が分かっていないので，ダイクストラ法を続行 *)
            | _ | exception Not_found ->
                q := WMap.remove w !q;
                Fun.flip List.iter us (fun u ->
                  match VArray.find d u with
                  | (d, _) when 0 < W.compare w d -> ()
                  | (_, pu) ->
                      (* 未だ頂点uを訪れていない *)
                      Fun.flip (es u).fold () @@ fun (v, c, e) () ->
                        let dv' = let open W in w + c in
                        match VArray.find d v with
                        | (dv, _) when W.compare dv dv' < 0 -> ()
                        | (dv, pv) when W.compare dv dv' = 0 ->
                            (* 新しい最短経路を見つけたので追加 *)
                            let pv' = P.union pv (P.snoc pu e) in
                            if pv != pv' then VArray.update d v (dv, pv')
                        | _ | exception Not_found ->
                            VArray.update d v (dv', P.snoc pu e);
                            q := WMap.update dv' (fun vs -> Some (v :: Option.value ~default:[] vs)) !q);
                min_binding_opt := WMap.min_binding_opt !q;
                dijkstra_aux t in
      (fun v -> fst @@ dijkstra_aux v),
      (fun v -> snd @@ dijkstra_aux v)
  end

  module ByArray = struct
    module Make (W : sig include Weight val inf : t end) (P : sig include Path val empty : t end) = struct
      type 'a church_list = { fold : 'b. ('a -> 'b -> 'b) -> 'b -> 'b }

      module WMap = Map.Make (W)

      let dijkstra n es s =
        let d = Array.make n W.inf in
        let r = Array.make n P.empty in
        let q = ref (WMap.singleton W.zero [s]) in
        (* 既に最短距離が確定した辺へのクエリを高速化するため，
           ヒープの最小要素をメモしておく *)
        let min_binding_opt = ref (Some (W.zero, [s])) in
        r.(s) <- P.nil;
        d.(s) <- W.zero;
        let dijkstra_aux w us =
          q := WMap.remove w !q;
          List.iter (fun u ->
            if 0 <= W.compare d.(u) w then
              (* 未だ頂点uを訪れていない *)
              Fun.flip (es u).fold () @@ fun (v, c, e) () ->
                let dv' = let open W in w + c in
                match W.compare d.(v) dv' with
                | x when x < 0 -> ()
                (* 新しい最短経路を見つけたので追加 *)
                | 0 -> r.(v) <- P.union r.(v) (P.snoc r.(u) e)
                | _ ->
                    d.(v) <- dv';
                    r.(v) <- P.snoc r.(u) e;
                    q := WMap.update dv' (fun vs -> Some (v :: Option.value ~default:[] vs)) !q) us;
          min_binding_opt := WMap.min_binding_opt !q in
        let rec distance t =
          match !min_binding_opt with
          (* もう既に全ての頂点までの距離が分かっている *)
          | None -> d.(t)
          | Some (w, us) ->
              if W.compare d.(t) w <= 0
              (* 既に終点までの距離が分かっているので返す *)
              then d.(t)
              (* 終点までの距離が分かっていないので，ダイクストラ法を続行 *)
              else (dijkstra_aux w us; distance t) in
        let rec path t =
          match !min_binding_opt with
          (* もう既に全ての頂点までの最短経路が分かっている *)
          | None -> r.(t)
          | Some (w, us) ->
              if W.compare d.(t) w < 0
              (* 既に終点までの全ての最短経路が分かっているので返す *)
              then r.(t)
              (* 終点までの距離が分かっていないので，ダイクストラ法を続行 *)
              else (dijkstra_aux w us; path t) in
        distance, path

      let dijkstra_dense n es s =
        let d = Array.make n W.inf in
        let r = Array.make n P.empty in
        r.(s) <- P.nil;
        d.(s) <- W.zero;
        let rec dijkstra_dense_aux = function
          | [] -> ()
          | v :: vs ->
              let u, us = List.fold_left (fun (u, us) v ->
                if W.compare d.(u) d.(v) < 0
                then (u, v :: us)
                else (v, u :: us)) (v, []) vs in
              (es u).fold (fun (v, c, e) () ->
                let dv' = let open W in d.(u) + c in
                match W.compare d.(v) dv' with
                | x when x < 0 -> ()
                | 0 -> r.(v) <- P.union r.(v) (P.snoc r.(u) e)
                | _ -> d.(v) <- dv'; r.(v) <- P.snoc r.(u) e) ();
              dijkstra_dense_aux us in
        dijkstra_dense_aux (List.init n Fun.id);
        (Array.get d, Array.get r)
      end
  end

  module ByHashtbl = struct
    module Make (V : Hashtbl.HashedType) (W : Weight) (P : Path) = struct
      module VHash = Hashtbl.Make (V)
      module C = Core (W) (P) (struct
        type t = (W.t * P.t) VHash.t
        type vertex = V.t
        let find = VHash.find
        let update = VHash.replace
      end)
      include C

      let dijkstra n e s = C.dijkstra (VHash.create n) e s
    end
  end

  module ByMap = struct
    module Make (V : Map.OrderedType) (W : Weight) (P : Path) = struct
      module VMap = Map.Make (V)
      module C = Core (W) (P) (struct
        type t = (W.t * P.t) VMap.t ref
        type vertex = V.t
        let find d v = VMap.find v !d
        let update d v w = d := VMap.add v w !d
      end)
      include C

      let dijkstra e s = C.dijkstra (ref VMap.empty) e s
    end
  end
end

module IntWeightedDirectedGraphByArray = struct
  module Make 
    (Path : sig
      type t
      type edge
      val nil : t
      val empty : t
      val snoc : t -> edge -> t
      val union : t -> t -> t
    end)
  = struct
    include (WeightedDirectedGraph.ByArray.Make (struct
      type t = int
      let zero = 0
      let inf = max_int
      let ( + ) = ( + )
      let compare = compare
    end) (Path))

    (* 辺の重さがごく小さい整数の場合は，Mapをキュー代わりに使うより配列を使った方が効率的
       時間計算量はO(E+VW)，空間計算量はO(E+W) *)
    let dijkstra_special :
      (* 頂点数n *)
      int ->
      (* 辺の重さの上限W *)
      int ->
      (* 頂点uを受け取って，
         uから伸びる辺の終点vと重さwの組みのリストを返す関数
         辺の重さwはW以下でなくてはならない *)
      (int -> (int * int * Path.edge) church_list) ->
      (* 始点 *)
      int ->
      (* 始点からの最短距離を返す関数と，始点からの最短経路を返す関数の組
         始点から辿り着けない場合，前者はinfを，後者はPath.emptyを返す
         これらの関数を覚えておけば，呼び出しごとの途中までの計算結果がシェアされる *)
      (int -> int) * (int -> Path.t)
    = fun n mw es s ->
      (* 各頂点への最短距離を格納するやつ *)
      let d = Array.make n max_int in
      (* 各頂点への最短経路を格納するやつ *)
      let r = Array.make n Path.nil in
      (* キュー *)
      let q = Array.make (mw + 1) [] in
      (* ヒープの最小要素の重み *)
      let w = ref 0 in
      (* ヒープの最小要素がどのインデックスに対応するか *)
      let i = ref 0 in
      (* ヒープに格納されている要素の数 *)
      let m = ref 1 in
      d.(s) <- 0;
      q.(0) <- [s];
      let dijkstra_aux () =
        match q.(!i) with
        (* ヒープには重さがwの要素は無かった *)
        | [] ->
            incr w;
            incr i;
            if mw < !i then i := 0
        (* ヒープには重さwの要素uが存在する *)
        | u :: us ->
            (* ヒープから重さwの要素を削除する *)
            decr m;
            q.(!i) <- us;
            if !w <= d.(u) then
              (* 未だ頂点uを訪れていない *)
              (es u).fold (fun (v, c, e) () ->
                match compare d.(v) (!w + c) with
                | x when x < 0 -> ()
                | 0 -> r.(v) <- Path.union r.(v) (Path.snoc r.(u) e);
                | _ ->
                    d.(v) <- !w + c; (* 頂点vの重さを緩和 *)
                    r.(v) <- Path.snoc r.(u) e;
                    (* ヒープにvを追加する *)
                    let j = !i + c - if !i + c <= mw then 0 else mw + 1 in
                    q.(j) <- v :: q.(j);
                    incr m) () in
      let rec distance t =
        if !m <= 0 || d.(t) <= !w
        (* 既に終点までの距離が分かっているので返す *)
        then d.(t)
        else (dijkstra_aux (); distance t) in
      let rec path t =
        if !m <= 0 || d.(t) < !w
        (* 既に終点までの最短経路が全て分かっているので返す *)
        then r.(t)
        else (dijkstra_aux (); path t) in
      distance, path
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

module G = IntWeightedDirectedGraphByArray.Make
  (struct
    type t = string list
    type edge = string
    let nil = []
    let empty = []
    let union = Fun.const
    let snoc = Fun.flip List.cons
  end)

let e =
  [|[ (1, 7); (2, 9); (5, 14) ];
    [ (0, 7); (2, 10); (3, 15) ];
    [ (0, 9); (1, 10); (3, 11); (5, 2) ];
    [ (1, 15); (2, 11); (4, 6) ];
    [ (3, 6); (5, 9) ];
    [ (0, 14); (2, 2); (4, 9) ]|];;

let d, r = Fun.flip (G.dijkstra 6) 0 @@ fun u ->
  { G.fold = fun f ->
    List.fold_right (fun (v, w) -> f (v, w, Printf.sprintf "%d->%d" u v)) e.(u) };;
List.init 6 d;;
List.init 6 r;;

let d, r = Fun.flip (G.dijkstra_dense 6) 0 @@ fun u ->
  { G.fold = fun f ->
    List.fold_right (fun (v, w) -> f (v, w, Printf.sprintf "%d->%d" u v)) e.(u) };;
List.init 6 d;;
List.init 6 r;;

let d, r = Fun.flip (G.dijkstra_special 6 16) 0 @@ fun u ->
  { G.fold = fun f ->
    List.fold_right (fun (v, w) -> f (v, w, Printf.sprintf "%d->%d" u v)) e.(u) };;
List.init 6 d;;
List.init 6 r;;

(* 最短経路の数を数える *)
module G = IntWeightedDirectedGraphByArray.Make
  (struct
    type t = int
    type edge = unit
    let nil = 1
    let empty = 0
    let union = ( + )
    let snoc = Fun.const
  end);;

let e =
  [|[ (1, 1); (2, 2); (3, 3) ];
    [ (5, 3) ];
    [ (5, 2) ];
    [ (4, 1) ];
    [ (5, 0) ]; []|];;

let d, r = Fun.flip (G.dijkstra 6) 0 @@ fun u ->
  { G.fold = fun f ->
    List.fold_right (fun (v, w) -> f (v, w, ())) e.(u) };;
List.init 6 d;;
List.init 6 r;;

let d, r = Fun.flip (G.dijkstra_dense 6) 0 @@ fun u ->
  { G.fold = fun f ->
    List.fold_right (fun (v, w) -> f (v, w, ())) e.(u) };;
List.init 6 d;;
List.init 6 r;;

let d, r = Fun.flip (G.dijkstra_special 6 3) 0 @@ fun u ->
  { G.fold = fun f ->
    List.fold_right (fun (v, w) -> f (v, w, ())) e.(u) };;
List.init 6 d;;
List.init 6 r;;

(* 無限グラフ!!! *)
module IntPair = struct
  type t = int * int
  let compare = compare
end

module G = WeightedDirectedGraph.ByMap.Make (IntPair) (struct
  type t = int
  let inf = max_int 
  let zero = 0
  let ( + ) = ( + )
  let compare = compare
end)
(* 経路の情報は保存しない *)
(struct
  type t = unit
  type edge = unit
  let nil = ()
  let snoc _ _ = ()
  let union = Fun.const
end)

let d, _ = Fun.flip G.dijkstra (0, 0) @@ fun (x, y) ->
  { G.fold = fun f ->
    List.fold_right (fun v -> f (v, abs x + abs y, ()))
    [(x + 1, y); (x - 1, y); (x, y + 1); (x, y - 1)] };;
List.init 10 @@ fun x ->
  List.init 10 @@ fun y -> d (x, y);;
