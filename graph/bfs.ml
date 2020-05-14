module DirectedGraph
: sig
  (* 配列を用いたBFSの実装 *)
  module ByArray : sig
    module Make :
      functor
        (* 道の表現 *)
        (Path : sig
          type t
          (* 辺の名前 *)
          type edge
          (* 長さ0の道 *)
          val nil : t
          (* 道の後ろに辺を付け足した道 *)
          val snoc : t -> edge -> t
        end) ->
        sig
          type 'a church_list = { fold : 'b. ('a -> 'b -> 'b) -> 'b -> 'b }

          (* BFSにより，重みのないグラフの最短経路を求める *)
          val bfs :
            (* 頂点数n *)
            int ->
            (* 辺の名前が付いた隣接リスト *)
            (int -> (int * Path.edge) church_list) ->
            (* 始点 *)
            int ->
            (* 始点からの最短経路を返す関数
               この関数を覚えておけば，呼び出しごとの途中までの計算結果がシェアされる *)
            (int -> Path.t option)
        end
  end

  (* ハッシュテーブルを用いたBFSの実装 *)
  module ByHashtbl : sig
    module Make :
      functor
        (* 頂点をキーとしたハッシュテーブルの実装 *)
        (VHash : Hashtbl.S)
        (* 道の表現 *)
        (Path : sig
          type t
          (* 辺の名前 *)
          type edge
          (* 長さ0の道 *)
          val nil : t
          (* 道の後ろに辺を付け足した道 *)
          val snoc : t -> edge -> t
        end) ->
        sig
          type 'a church_list = { fold : 'b. ('a -> 'b -> 'b) -> 'b -> 'b }

          (* BFSにより，重みのないグラフの最短経路を求める *)
          val bfs :
            (* 頂点数（ハッシュテーブルを用いるので目安程度） *)
            int ->
            (* 辺の名前が付いた隣接リスト *)
            (VHash.key -> (VHash.key * Path.edge) church_list) ->
            (* 始点 *)
            VHash.key ->
            (* 最短経路を返す関数 辿り着けなければNoneを返す） *)
            (VHash.key -> Path.t option)
        end
  end

  (* Mapを用いたBFSの実装 *)
  module ByMap : sig
    module Make :
      functor
        (* 頂点をキーとしたMapの実装 *)
        (VMap : Map.S)
        (* 道の表現 *)
        (Path : sig
          type t
          (* 辺の名前 *)
          type edge
          (* 長さ0の道 *)
          val nil : t
          (* 道の後ろに辺を付け足した道 *)
          val snoc : t -> edge -> t
        end) ->
        sig
          type 'a church_list = { fold : 'b. ('a -> 'b -> 'b) -> 'b -> 'b }

          (* BFSにより，重みのないグラフの最短経路を求める *)
          val bfs :
            (* 辺の名前が付いた隣接リスト *)
            (VMap.key -> (VMap.key * Path.edge) church_list) ->
            (* 始点 *)
            VMap.key ->
            (* 最短経路を返す関数 辿り着けなければNoneを返す） *)
            (VMap.key -> Path.t option)
        end
  end
end
= struct
  module type Path = sig
    type t
    (* 辺の名前 *)
    type edge
    (* 長さ0の道 *)
    val nil : t
    (* 道の後ろに辺を付け足した道 *)
    val snoc : t -> edge -> t
  end

  (* 最短経路を格納するデータ構造を抽象化したBFSの実装 *)
  module Core
    (P : Path)
    (* グラフの頂点を添字としたハッシュ *)
    (VHash : sig
      type t
      type vertex (* グラフの頂点 *)
      val find_opt : t -> vertex -> P.t option
      val update : t -> vertex -> P.t -> unit
    end)
  = struct
    type 'a church_list = { fold : 'b. ('a -> 'b -> 'b) -> 'b -> 'b }

    let bfs d es s =
      VHash.update d s P.nil;
      let q = ref [s] in
      let rec bfs_aux t =
        match VHash.find_opt d t, !q with
        (* もう既に全ての頂点までの経路が分かっている *)
        | None, [] -> None
        (* 既に終点までの経路が分かっているので返す *)
        | Some _ as ans, _ -> ans
        (* 終点までの経路が分かっていないので，BFSを続行 *)
        | None, _ :: _ ->
            q := List.fold_left (fun acc u ->
              let Some p = VHash.find_opt d u in
              Fun.flip (es u).fold acc @@ fun (v, e) q ->
                match VHash.find_opt d v with
                | Some _ -> q
                | None -> VHash.update d v (P.snoc p e); v :: q) [] !q;
            bfs_aux t in
      bfs_aux
  end

  module ByArray = struct
    module Make (P : Path) = struct
      module C = Core (P) (struct
        type t = P.t option array
        type vertex = int
        let find_opt = Array.get
        let update d v p = d.(v) <- Some p
      end)
      include C

      let bfs n e s = C.bfs (Array.make n None) e s
    end
  end

  module ByHashtbl = struct
    module Make (VHash : Hashtbl.S) (P : Path) = struct
      module C = Core (P) (struct
        type t = P.t VHash.t
        type vertex = VHash.key
        let update = VHash.add
        let find_opt = VHash.find_opt
      end)
      include C

      let bfs n e s = C.bfs (VHash.create n) e s
    end
  end

  module ByMap = struct
    module Make (VMap : Map.S) (P : Path) = struct
      module C = Core (P) (struct
        type t = P.t VMap.t ref
        type vertex = VMap.key
        let find_opt d v = VMap.find_opt v !d
        let update d v p = d := VMap.add v p !d
      end)
      include C

      let bfs e s = C.bfs (ref VMap.empty) e s
    end
  end
end

module IntPairHash = Hashtbl.Make
  (struct
    type t = int * int
    let equal = ( = )
    let hash = Hashtbl.hash
  end)

(* 経路情報も欲しい場合 *)
module G = DirectedGraph.ByHashtbl.Make
(IntPairHash)
(struct
  type t = string list
  type edge = string
  let nil = []
  let snoc t e = e :: t
end)

let maze =
  [|"......";
    ".#####";
    "..#.#.";
    "..##..";
    "#....."|];;

let d = G.bfs 30 (fun (i, j) ->
  { G.fold = fun f ->
    List.fold_right (fun (i', j') acc ->
      match maze.(j').[i'] = '.' with
      | false | exception (Invalid_argument _) -> acc
      | true -> f ((i', j'), Printf.sprintf "(%d,%d)->(%d,%d)" i j i' j') acc)
    [(i - 1, j); (i + 1, j); (i, j - 1); (i, j + 1)] }) (0, 0);;
d (5, 0);;
(* 途中までの計算結果が再利用される *)
d (3, 2);;

(* 距離だけ欲しい場合 *)
module G = DirectedGraph.ByHashtbl.Make (IntPairHash)
(struct
  type t = int
  type edge = unit
  let nil = 0
  let snoc t _ = t + 1
end)

let d = G.bfs 30 (fun (i, j) ->
  { G.fold = fun f ->
    List.fold_right (fun (i, j) acc ->
      match maze.(j).[i] = '.' with
      | false | exception (Invalid_argument _) -> acc
      | true -> f ((i, j), ()) acc)
    [(i - 1, j); (i + 1, j); (i, j - 1); (i, j + 1)] }) (0, 0);;
d (5, 0);;
d (3, 2);;
