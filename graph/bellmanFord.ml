module WeightedDirectedGraph
  (Vertex : sig
    type t
    val compare : t -> t -> int
  end)
  (Weight : sig
    type t
    val zero : t
    val ( + ) : t -> t -> t
    val compare : t -> t -> int
  end) :
sig
  (* 配列版の実装 *)
  val raw_bellman_ford :
    (* 頂点数n *)
    int ->
    (* 辺のリスト *)
    (* 頂点は0からn-1までの整数でなくてはならない *)
    (int * int * Weight.t) list ->
    (* 始点 *)
    int ->
    (int -> [ `Weight of Weight.t | `Inf | `NegInf ])

  (* 座標圧縮により様々な型を使えるようにしたバージョン *)
  val bellman_ford :
    (* 辺のリスト *)
    (Vertex.t * Vertex.t * Weight.t) list ->
    (* 始点 *)
    Vertex.t ->
    (Vertex.t -> [ `Weight of Weight.t | `Inf | `NegInf ])
end =
struct
  module CC = CoordComp (Vertex)
  module Weight' = WithInf (Weight)

  let raw_bellman_ford n es s =
    (* 距離を覚えるやつ *)
    let d = Array.make n Weight'.inf in
    (* 経路に負閉路が含まれる頂点を覚えるやつ *)
    let neg = Array.make n false in
    d.(s) <- Weight'.zero;
    for i = 0 to 2 * n - 1 do
      List.iter (fun (u, v, c) ->
        let open Weight' in
        (* c は u から v への辺の重さ *)
        (* d.(u) + c < d.(v) *)
        if 0 < compare d.(v) (d.(u) + Some c) then begin
          d.(v) <- d.(u) + Some c;
          if n - 1 <= i then
            (* n 回目以降に変更が起こった場合，v までの経路に負閉路が含まれている *)
            neg.(v) <- true
        end) es
    done;
    fun v ->
      if neg.(v) then `NegInf
      else
        match d.(v) with
        | None -> `Inf
        | Some d -> `Weight d

  let bellman_ford es s =
    let (n, comp, _) =
      CC.compress @@
        List.concat @@
          List.map (fun (u, v, _) -> [u; v]) es in
    match
      raw_bellman_ford n
        (List.map (fun (u, v, c) -> (comp u, comp v, c)) es) (comp s)
    with
    | exception Not_found -> (* 始点から延びる辺がない *)
        fun _ -> `Inf
    | d ->
        fun v -> try d (comp v) with Not_found -> `Inf
end

(* sample code *)

module Int = struct
  type t = int
  let zero = 0
  let ( + ) = ( + )
  let compare = compare
end

module G = WeightedDirectedGraph (Int) (struct
  type t = float
  let zero = 0.
  let ( + ) = ( +. )
  let compare = compare
end)

let d = G.raw_bellman_ford 8
  [ (1, 2, 1.); (2, 3, 1.); (3, 7, 1.); (4, 5, -1.); (5, 6, -1.); (6, 4, -1.) ] 1;;
Array.init 8 d;;

let d = G.raw_bellman_ford 8
  [ (1, 2, 1.); (2, 3, 1.); (3, 7, 1.); (4, 5, -1.); (5, 6, -1.); (6, 4, -1.); (7, 4, -1.) ] 1;;
Array.init 8 d;;

let d = G.raw_bellman_ford 8
  [ (1, 2, 1.); (2, 3, 1.); (3, 7, 1.); (4, 5, -1.); (5, 6, -1.); (6, 4, -1.); (7, 4, -1.); (4, 7, 1.) ] 1;;
Array.init 8 d;;

let d = G.bellman_ford
  (List.map (fun (u, v, c) -> (u * 10, v * 10, c))
  [ (1, 2, 1.); (2, 3, 1.); (3, 7, 1.); (4, 5, -1.); (5, 6, -1.); (6, 4, -1.) ]) 10;;
Array.init 8 (fun v -> d @@ v * 10);;

let d = G.bellman_ford
  (List.map (fun (u, v, c) -> (u * 10, v * 10, c))
  [ (1, 2, 1.); (2, 3, 1.); (3, 7, 1.); (4, 5, -1.); (5, 6, -1.); (6, 4, -1.); (7, 4, -1.) ]) 10;;
Array.init 8 (fun v -> d @@ v * 10);;

let d = G.bellman_ford
  (List.map (fun (u, v, c) -> (u * 10, v * 10, c))
  [ (1, 2, 1.); (2, 3, 1.); (3, 7, 1.); (4, 5, -1.); (5, 6, -1.); (6, 4, -1.); (7, 4, -1.); (4, 7, 1.) ]) 1;;
Array.init 8 (fun v -> d @@ v * 10);;
