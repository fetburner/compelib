module DirectedGraph
  (* 頂点を添字，経路長を要素とした配列の実装 *)
  (Array : sig
    type t
    type key
    type elt = int (* 重みのない（=全ての重さが1な）グラフなので経路長は非負整数 *)
    val get : t -> key -> elt
    val set : t -> key -> elt -> unit
  end)
: sig
  type vertex = Array.key

  (* BFSにより，重みのないグラフの最短経路長を求める *)
  val shortest_path :
    (* 全ての頂点についての経路長が無限大で初期化された配列 *)
    Array.t ->
    (* 最短経路を求めたいグラフの，ある頂点から伸びる辺に対してのイテレータ *)
    (vertex -> (vertex -> unit) -> unit) ->
    (* 始点 *)
    vertex ->
    (* 終点を受け取って，始点からの最短距離を返す関数
       始点から辿り着けない場合，無限大を返す
       この関数を覚えておけば，呼び出しごとの途中までの計算結果がシェアされる *)
    (vertex -> int)
end
= struct
  type vertex = Array.key

  let shortest_path d es s =
    (* 始点への経路長を0にする *)
    Array.set d s 0;
    let w = ref 1 in
    let q = ref [s] in
    let rec bfs t =
      match !q with
      (* もう既に全ての頂点までの経路が分かっているので返す *)
      | [] -> Array.get d t
      | us ->
          match Array.get d t with
          (* 既に終点までの距離が分かっているので返す *)
          | x when x < !w -> x
          (* 終点までの距離が分かっていないので，BFSを続行 *)
          | _ ->
              q := [];
              List.iter (fun u ->
                es u @@ fun v ->
                  if !w < Array.get d v then
                    (q := v :: !q; Array.set d v !w)) us;
              incr w;
              bfs t in
    bfs
end

(* 使用例 *)

module G = DirectedGraph (struct
  type t = (int, Bigarray.int_elt, Bigarray.c_layout) Bigarray.Genarray.t
  type elt = int
  type key = int array
  let get = Bigarray.Genarray.get
  let set = Bigarray.Genarray.set
end)

let maze =
  [|"......";
    ".#####";
    "..#.#.";
    "..##..";
    "#....."|];;

let d = G.shortest_path
  (let d = Bigarray.Genarray.create Bigarray.int Bigarray.c_layout [| 6; 5 |] in Bigarray.Genarray.fill d max_int; d)
  (fun [| i; j |] f ->
    List.iter (fun ([| i; j |] as v) ->
      match maze.(j).[i] = '.' with
      | false | exception (Invalid_argument _) -> ()
      | true -> f v)
    [ [| i + 1; j |]; [| i - 1; j |]; [| i; j + 1 |]; [| i; j - 1 |] ]) [| 0; 0 |];;

d [| 5; 0 |];;
d [| 3; 2 |];;

(* このBFSを用いるのとダイクストラ法を用いるのとの計算量の違いは，
   ヒープを適切に選べば定数倍でしかないので，経路復元がしたいならダイクストラ法の実装を用いること *)
